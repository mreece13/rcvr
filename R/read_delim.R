#' Merge ballots that a vendor split across several rows (multi-card ballots)
#'
#' The wide-frame form of the card linking that `pairs_from_delim()` applies:
#' see `fragment_links()` for the rule. A continuation card row is merged
#' into the row of the voter's first card; rows that cannot be linked stay
#' separate (warning `rcvr_unlinked_cards`).
#'
#' @param raw A wide CVR tibble.
#' @param path The source path, used only in the warning message.
#'
#' @return `raw`, with continuation rows merged into their ballot.
fix_fragmentation <- function(raw, path = NA) {
  if (!("ballot_style" %in% colnames(raw)) || nrow(raw) < 2) {
    return(raw)
  }
  layout <- delim_layout(colnames(raw))
  if (length(layout$value_cols) < 2) return(raw)

  fr <- fragment_links(delim_cells(raw, layout), path)
  if (is.null(fr) || !any(fr$linked)) return(raw)

  raw <- raw[fr$o, , drop = FALSE]
  cont <- fr$linked
  grp <- cumsum(!cont)
  main_row <- which(!cont)[grp]
  for (col in colnames(raw)[layout$value_cols]) {
    v <- raw[[col]]
    filled <- !is.na(v) & v != ""
    take <- cont & filled
    if (any(take)) {
      tgt <- main_row[take]
      blank_tgt <- !filled[tgt]
      v[tgt[blank_tgt]] <- v[take][blank_tgt]
      raw[[col]] <- v
    }
  }
  raw[!cont, , drop = FALSE]
}

#' Fold a second header row of candidate names into the column names
#'
#' Some exports (Hawaii 2024) write a contest-name header row and then a
#' candidate-name row before the ballots:
#'
#'     CVRNumber,BatchId,Precinct,President,President,...
#'     ,,,(R) TRUMP...,(D) HARRIS...,...
#'     1,-1,48-02VSC,1,,...
#'
#' The second row is recognised by its key cells (CVR number, batch,
#' precinct, ...) all being blank while most contest cells hold non-numeric
#' text. Each contest column is renamed `contest||candidate` and the row is
#' dropped.
#'
#' @param raw A `data.table` straight from the reader, key columns already
#'   renamed to their canonical names.
#'
#' @return `raw`, possibly renamed and one row shorter.
merge_candidate_row <- function(raw) {
  if (nrow(raw) < 2) return(raw)
  key <- c(rcvr_DROP_COLS, unname(rcvr_RENAME_COLS), unique(names(rcvr_RENAME_COLS)))
  is_key <- colnames(raw) %in% key
  # precinct/ballot_style may be synthesised all-NA columns, which say nothing
  real_key <- is_key & !vapply(raw, function(x) all(is.na(x)), logical(1))
  if (!any(real_key) || all(is_key)) return(raw)

  first <- vapply(raw, function(x) as.character(x[1]), character(1))
  blank <- is.na(first) | first == ""
  val <- first[!is_key & !blank]
  if (!all(blank[real_key]) ||
      length(val) < 0.5 * sum(!is_key) ||
      any(stringr::str_detect(val, "^\\s*-?[\\d.]+\\s*$"))) {
    return(raw)
  }

  fill <- !is_key & !blank
  colnames(raw)[fill] <- paste0(colnames(raw)[fill], "||", first[fill])
  raw[-1]
}

#' Read an .xlsx CVR's first sheet as a grid of text cells with duckdb
#'
#' `readxl` parses the whole sheet into memory before it applies `n_max`, so
#' large exports cost far more than their size (Dane WI 2025 Spring: 17 GB
#' for one 139 MB file). duckdb's `excel` extension streams the sheet. Every
#' cell comes back as text, with no header handling: row 1 of the grid is row
#' 1 of the sheet, as `readxl::read_excel(col_names = FALSE)` would give it.
#'
#' Used only when `duckdb` and `DBI` are installed and the `excel` extension
#' loads (it is downloaded once on first use). Set
#' `options(rcvr.xlsx_reader = "readxl")` to always use readxl.
#'
#' @param path Path to an `.xlsx` file.
#'
#' @return A `data.table` of character columns `V1..Vn`, or `NULL` to fall
#'   back to readxl.
read_xlsx_grid <- function(path) {
  if (!identical(getOption("rcvr.xlsx_reader", "duckdb"), "duckdb") ||
      !requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE)) {
    return(NULL)
  }

  con <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  loaded <- tryCatch(
    {
      DBI::dbExecute(con, "LOAD excel")
      TRUE
    },
    error = function(e) tryCatch(
      {
        DBI::dbExecute(con, "INSTALL excel")
        DBI::dbExecute(con, "LOAD excel")
        TRUE
      },
      error = function(e) FALSE
    )
  )
  if (!loaded) {
    cli::cli_inform(
      "duckdb's {.val excel} extension is unavailable; reading {.file {path}} with readxl.",
      .frequency = "once", .frequency_id = "rcvr_xlsx_reader"
    )
    return(NULL)
  }

  src <- DBI::dbQuoteString(con, as.character(path))
  xlsx <- function(range = NULL) {
    sprintf(
      "read_xlsx(%s, header = false, all_varchar = true, stop_at_empty = false%s)",
      src, if (is.null(range)) "" else sprintf(", range = '%s'", range)
    )
  }

  # Without a range, duckdb takes the table width from the first row, so a
  # data column under a blank header cell (a multi-seat continuation, Dane WI
  # Feb25) would be dropped; with an open range it reads to row 1,048,576.
  # So take the row count from an unranged read, count the non-empty cells of
  # every column in a range wider than the first row (widening until the
  # last column comes back empty), and read exactly the used width. The
  # sheet's <dimension> element cannot be used: some exports write "A1".
  grid <- tryCatch(
    {
      n <- max(DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s", xlsx()))$n, 1)
      w <- nrow(DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM %s", xlsx())))
      repeat {
        w <- min(2L * w + 16L, 16384L)
        cols <- vapply(seq_len(w), xlsx_col_letter, character(1))
        counts <- DBI::dbGetQuery(con, sprintf(
          "SELECT %s FROM %s",
          paste0("count(", DBI::dbQuoteIdentifier(con, cols), ")", collapse = ", "),
          xlsx(sprintf("A1:%s%d", cols[w], n))
        ))
        counts <- unlist(counts, use.names = FALSE)
        if (counts[w] == 0 || w >= 16384L) break
      }
      used <- max(c(1L, which(counts > 0)))
      DBI::dbGetQuery(con, sprintf(
        "SELECT * FROM %s", xlsx(sprintf("A1:%s%d", cols[used], n))
      ))
    },
    error = function(e) NULL
  )
  if (is.null(grid)) return(NULL)

  data.table::setDT(grid)
  data.table::setnames(grid, paste0("V", seq_along(grid)))
  # readxl drops trailing all-empty rows; duckdb's row count can include one
  keep <- logical(nrow(grid))
  for (j in seq_along(grid)) keep <- keep | !is.na(grid[[j]])
  last <- if (any(keep)) max(which(keep)) else 0L
  if (last < nrow(grid)) grid <- grid[seq_len(last)]
  grid
}

# Split a raw cell grid into a frame whose names are grid row `row` and whose
# rows are everything below it. A blank name becomes `V<j>`, as fread names
# a blank header cell, so resolve_placeholder_cols() treats both alike.
grid_with_header <- function(grid, row = 1L) {
  nms <- vapply(grid, function(x) as.character(x[row]), character(1), USE.NAMES = FALSE)
  blank <- is.na(nms) | nms == ""
  nms[blank] <- paste0("V", which(blank))
  out <- grid[-seq_len(row)]
  data.table::setnames(out, nms)
  out
}

# spreadsheet column letters for a 1-based column index (1 -> "A", 28 -> "AB")
xlsx_col_letter <- function(i) {
  s <- character(0)
  while (i > 0) {
    r <- (i - 1L) %% 26L
    s <- c(LETTERS[r + 1L], s)
    i <- (i - 1L) %/% 26L
  }
  paste(s, collapse = "")
}

#' Read a single delimited CVR file
#'
#' @param path Path to a CSV, XLS, or XLSX CVR.
#' @param n Number of file rows to read after the first header line. Used to
#'   read the header of a large file without reading the data.
#'
#' @return A wide tibble, one row per ballot, with `precinct` and
#'   `ballot_style` present (possibly all-`NA`).
#' @export
read_delim_cvr <- function(path, n = Inf) {
  path <- fs::path_real(path)

  # checked ahead of is_header(), which assumes a csv/xls/xlsx extension and
  # otherwise leaves its own `d` undefined -- an unrecognised extension must
  # produce this abort, not a bare "object 'd' not found"
  ext <- fs::path_ext(path) |> stringr::str_to_upper()
  if (!(ext %in% c("CSV", "XLS", "XLSX"))) {
    cli::cli_abort("{.file {path}} is not a CSV or Excel file")
  }

  # readxl holds the whole sheet in memory (a 139 MB xlsx peaks at ~17 GB),
  # so an .xlsx is read with duckdb when it is available
  grid <- if (ext == "XLSX") read_xlsx_grid(path) else NULL

  header <- if (is.null(grid)) {
    is_header(path)
  } else {
    is_header_cell(grid[[1]][1], if (ncol(grid) >= 2) grid[[2]][1] else NA_character_)
  }
  if (header) {
    raw <- header_processor(path, n = n, grid = grid)
  } else if (!is.null(grid)) {
    raw <- grid_with_header(grid, 1L)
    if (is.finite(n)) raw <- raw[seq_len(min(n, nrow(raw)))]
  } else {
    # names are read as written (fread keeps duplicates; readxl "minimal"
    # does not append `...NN` to them): a repeated contest name is resolved
    # by resolve_placeholder_cols() below, never by a column-index suffix
    raw <- switch(ext,
      "CSV" = read_csv_ragged(path, n = n),
      "XLS" = , "XLSX" = readxl::read_excel(path, col_types = "text", .name_repair = "minimal", n_max = n)
    )
    blank <- is.na(colnames(raw)) | colnames(raw) == ""
    colnames(raw)[blank] <- paste0("...", which(blank))
  }
  raw <- data.table::as.data.table(raw)
  if (!header) {
    n_read <- nrow(raw)
    raw <- merge_shuffled_header(raw)
  }
  n_file_cols <- ncol(raw)

  colnames(raw) <- iconv(colnames(raw), to = "UTF-8", sub = "")

  # Select one matching candidate for each canonical name defined in
  # `rcvr_RENAME_COLS`. We pick the first candidate that appears in the
  # file (priority determined by the ordering in `rcvr_RENAME_COLS`). This
  # ensures only one `ballot_style` column and one `precinct` column are
  # chosen when multiple variants exist.
  for (canonical in unique(names(rcvr_RENAME_COLS))) {
    candidates <- rcvr_RENAME_COLS[names(rcvr_RENAME_COLS) == canonical]
    found <- candidates[candidates %in% colnames(raw)]
    if (length(found) > 0) {
      old_name <- found[1]
      if (old_name != canonical) {
        colnames(raw)[colnames(raw) == old_name] <- canonical
      }
    } else if (!(canonical %in% colnames(raw))) {
      # a column already named `precinct` is kept, not blanked.
      # by reference: `[[<-` would run make.unique() over repeated names
      data.table::set(raw, j = canonical, value = rep(NA_character_, nrow(raw)))
    }
  }

  if (!header) raw <- merge_candidate_row(raw)

  # after the canonicalisation loop, so a canonical rename is in place and can
  # never be mistaken for a placeholder, and so a placeholder run can never
  # inherit a key column's name
  resolved <- resolve_placeholder_cols(colnames(raw))
  colnames(raw) <- resolved$names

  raw <- tibble::as_tibble(raw)
  attr(raw, "rcvr_magnitude") <- resolved$magnitude
  # for read_delim_chunked(): how many file lines precede the first ballot
  # row (header rows, and any header-like rows the steps above consumed),
  # and how many columns the file has before canonical columns are added
  if (is.finite(n)) {
    # a 4-row header (title, then three name rows in either order) is 4 lines
    attr(raw, "rcvr_skip") <- if (header) 4L else 1L + as.integer(n_read - nrow(raw))
    attr(raw, "rcvr_file_cols") <- n_file_cols
  }
  raw
}

#' Rebuild a Dominion header written in shuffled row order
#'
#' Pueblo CO 2022 writes the Dominion 4-row header as: key names with the
#' party of each candidate column, then the election title, then the contest
#' row, then the candidate row. Read as a plain CSV, the party row becomes the
#' column names (contests "DEM", "REP", ...) and the next three rows look like
#' ballots. Recognised by the title in the first cell of the first row, with
#' the contest row's key cells blank. Each candidate column is renamed
#' `contest||candidate||party`, the form `header_processor()` builds.
#'
#' @param raw A `data.table` straight from the reader.
#'
#' @return `raw`, possibly renamed and three rows shorter.
merge_shuffled_header <- function(raw) {
  if (nrow(raw) < 3 || !is_header_cell(as.character(raw[[1]][1]))) return(raw)
  cell <- function(i) vapply(raw, function(x) as.character(x[i]), character(1), USE.NAMES = FALSE)
  contest <- cell(2)
  candidate <- cell(3)
  is_contest <- !is.na(contest) & contest != ""
  if (!any(is_contest) || is_contest[1]) return(raw)

  nms <- colnames(raw)
  blank <- function(x) is.na(x) | x == ""
  party <- nms
  party[stringr::str_detect(nms, rcvr_PLACEHOLDER_RE) | blank(nms)] <- NA
  new <- ifelse(blank(candidate), contest, paste(contest, candidate, sep = "||"))
  new <- ifelse(is.na(party), new, paste(new, party, sep = "||"))
  nms[is_contest] <- new[is_contest]
  data.table::setnames(raw, make.unique(nms, sep = rcvr_DUP_SENTINEL))
  raw[-(1:3)]
}

#' Read a directory of delimited CVR files
#'
#' @param dir Path to a directory of CSV or Excel CVRs.
#'
#' @return A wide tibble, all files row-bound.
#' @export
read_delim_multi_cvr <- function(dir) {
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  # `read_delim_multi_cvr()` is public and callable directly, so it must be
  # safe to point at a directory holding a stray non-delim file (a JSON
  # note, a `Write In Images` subfolder) without aborting the whole county.
  delim_files <- files[stringr::str_detect(files, "\\.(csv|CSV|xls|XLS|xlsx|XLSX)$")]

  if (length(delim_files) == 0) {
    cli::cli_abort(
      "No delimited files found in {.file {dir}}.",
      class = "rcvr_no_files"
    )
  }

  raws <- lapply(delim_files, read_delim_cvr)

  out <- dplyr::bind_rows(raws)

  # `bind_rows()` drops the attribute, and two files in one directory may each
  # contribute a magnitude for the same contest. Take the widest run seen.
  mags <- unlist(lapply(raws, attr, "rcvr_magnitude"))
  attr(out, "rcvr_magnitude") <- if (length(mags) > 0) {
    vapply(split(as.integer(mags), names(mags)), max, integer(1))
  } else {
    integer(0)
  }

  out
}

#' Reshape a wide delimited CVR into the pairs frame
#'
#' @param raw A tibble from [read_delim_cvr()] or [read_delim_multi_cvr()].
#' @param path The source path, used only in warning messages.
#'
#' @return A pairs frame: `cvr_id`, `precinct`, `contest`, `raw_candidate`,
#'   `raw_party`, `rank`.
pairs_from_delim <- function(raw, path = NA) {
  layout <- delim_layout(colnames(raw))
  pairs_from_cells(delim_cells(raw, layout), layout, attr(raw, "rcvr_magnitude"), path)
}


#' Split delimited column names into contest, candidate, party, and rank
#'
#' Column names come in three shapes:
#'
#' * `contest` -- one column per contest, the cell holds the candidate.
#'   A trailing `": (id)"` contest id (Carson City NV) is dropped; any other
#'   colon is part of the name (`"Maui: Salary Commission"`).
#' * `contest||candidate||party` -- one column per candidate, the cell is a
#'   0/1 mark (Dominion 4-row headers, flattened by `header_processor()`).
#' * `Choice_<id>_<id>:contest:Vote For N:candidate:party` -- ClearCount's
#'   normalized export. `Vote For N` is folded into the contest as
#'   `"contest (Vote For=N)"`, the form ClearCount's wide export uses.
#'
#' A ranked-choice contest (`"... Number of ranks=7)"`, Larimer CO) writes the
#' rank as a `"(n)"` suffix on the candidate; it is moved into `rank`.
#'
#' @param nms Column names, any dedup sentinel already removed.
#'
#' @return A list of equal-length vectors `contest`, `candidate`, `party`
#'   (character, `NA` when absent) and `rank` (integer).
parse_contest_cols <- function(nms) {
  choice_re <- stringr::regex("^Choice_\\d+_\\d+:", ignore_case = TRUE)
  choice <- stringr::str_detect(nms, choice_re)
  s <- stringr::str_remove(nms, choice_re)
  s[!choice] <- stringr::str_remove(s[!choice], ":\\s*\\(\\d+\\)$")

  parts <- vector("list", length(s))
  parts[choice] <- strsplit(s[choice], ":", fixed = TRUE)
  parts[!choice] <- strsplit(s[!choice], "||", fixed = TRUE)

  n <- length(s)
  contest <- candidate <- party <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    p <- parts[[i]]
    if (length(p) == 0) next
    if (choice[i] && length(p) >= 2 && grepl("^Vote For", p[2])) {
      vf <- sub("^Vote For\\s*=?\\s*", "Vote For=", p[2])
      p <- c(paste0(p[1], " (", vf, ")"), p[-(1:2)])
    }
    m <- length(p)
    contest[i] <- p[1]
    if (choice[i]) {
      if (m == 2) candidate[i] <- p[2]
      if (m >= 3) {
        candidate[i] <- paste(p[2:(m - 1)], collapse = ":")
        party[i] <- p[m]
      }
    } else {
      if (m >= 2) candidate[i] <- p[2]
      if (m >= 3) party[i] <- paste(p[3:m], collapse = "||")
    }
  }

  rank <- rep(NA_integer_, n)
  rcv <- !is.na(candidate) & stringr::str_detect(contest, stringr::regex("Number of ranks\\s*=", TRUE))
  ranked <- rcv & stringr::str_detect(candidate, "\\(\\d+\\)\\s*$")
  rank[ranked] <- as.integer(stringr::str_match(candidate[ranked], "\\((\\d+)\\)\\s*$")[, 2])
  candidate[ranked] <- stringr::str_trim(stringr::str_remove(candidate[ranked], "\\s*\\(\\d+\\)\\s*$"))

  list(
    contest = dplyr::na_if(contest, ""),
    candidate = dplyr::na_if(candidate, ""),
    party = dplyr::na_if(party, ""),
    rank = rank
  )
}

#' Read a one-row-header CSV whose rows and header may differ in width
#'
#' Two shapes `fread()` misreads on its own, both from Ohio 2020 exports:
#'
#' * Ragged-right rows (Pickaway OH): a row omits its empty trailing cells,
#'   so rows are 24 to 27 fields against a 27-field header. `fread()` sizes
#'   the table from the first rows and stops early at the first wider row.
#'   Read with `fill = TRUE`, short rows are padded.
#' * A header with unquoted commas inside its names (Fayette OH): a levy
#'   title like `Madison Township (Renewal) - Fire, .9 mill, 5 years` splits
#'   into three header fields, so the header has 40 fields against 29 in
#'   every row, and `fread()` took the first ballot row as the header. The
#'   commas inside a name are followed by a space and the separators are
#'   not, so the header is re-split on bare commas when that gives exactly
#'   the width the rows use and the columns past it are empty.
#'
#' A row wider than the header is not padded into unnamed columns, which
#' `resolve_placeholder_cols()` would read as extra seats of the last
#' contest; it aborts with `rcvr_bad_delim`.
#'
#' @param path Path to a CSV.
#' @param n Rows to read after the header.
#'
#' @return A `data.table` of character columns.
#' @noRd
read_csv_ragged <- function(path, n = Inf) {
  # the header line's own fields: fread(nrows = 0) still sizes the table
  # from the data rows
  line <- readLines(path, n = 1L, warn = FALSE)
  # byte-wise: a header that is not valid UTF-8 (Okaloosa FL, Broward FL,
  # Fort Bend TX, Collin TX 2020) makes a character-wise sub() abort
  line <- sub("^\xef\xbb\xbf", "", line, useBytes = TRUE)
  # Walton FL 2020 is tab-separated despite its .csv name (and has commas
  # inside contest names): a header with more tabs than commas is split on tabs
  n_in <- function(ch) lengths(regmatches(line, gregexpr(ch, line, fixed = TRUE, useBytes = TRUE)))
  sep <- if (n_in("\t") > n_in(",")) "\t" else ","
  raw <- data.table::fread(path, colClasses = "character", header = TRUE,
                           nrows = n, sep = sep, fill = TRUE)
  hdr <- unlist(data.table::fread(text = line, header = FALSE, sep = sep,
                                  colClasses = "character", fill = TRUE)[1],
                use.names = FALSE)
  # Rows wider than the header (Mono CA 2020: 18 to 41 fields against 34)
  # cannot be matched to contests by position, and fread() then also fails
  # to split the file at all, returning fewer columns than the header has.
  # Unquoted commas inside candidate names (Ross OH 2020: "Rep George W.
  # Lavender, Jr. (71GLAVENDER71)") make rows wider than the header in the
  # same way. As for the Fayette header below, a comma inside a name is
  # followed by a space and a separator is not: the rows are read again with
  # each ", " protected, and that read is kept only if it fits the header.
  if (ncol(raw) != length(hdr) && sep == ",") {
    fixed <- read_protecting_comma_space(path, n, length(hdr))
    if (!is.null(fixed)) raw <- fixed
  }
  if (ncol(raw) != length(hdr)) {
    cli::cli_abort(
      "{.file {path}}: read as {ncol(raw)} column{?s} against a {length(hdr)}-field header; rows are wider than the header, so cells cannot be matched to contests.",
      class = "rcvr_bad_delim"
    )
  }
  used <- vapply(raw, function(x) any(!is.na(x) & x != ""), logical(1))
  width <- max(c(0L, which(used)))
  if (width < ncol(raw) && sep == ",") {
    bare <- if (validUTF8(line)) strsplit(line, ",(?! )", perl = TRUE)[[1]] else character(0)
    if (length(bare) %in% c(width, width + 1L) && length(bare) < ncol(raw)) {
      bare <- gsub('^"|"$', "", bare)
      keep <- seq_len(length(bare))
      raw <- raw[, keep, with = FALSE]
      data.table::setnames(raw, bare)
    }
  }
  raw
}

# Read a CSV with every ", " inside a cell kept as text, for
# read_csv_ragged(). Returns NULL unless the result is exactly `width`
# columns wide.
read_protecting_comma_space <- function(path, n, width) {
  lines <- readLines(path, n = if (is.finite(n)) n + 1L else -1L, warn = FALSE)
  lines[1] <- sub("^\xef\xbb\xbf", "", lines[1], useBytes = TRUE)
  mark <- "\x01"
  if (any(grepl(mark, lines, fixed = TRUE, useBytes = TRUE))) return(NULL)
  lines <- gsub(", ", paste0(mark, " "), lines, fixed = TRUE, useBytes = TRUE)
  raw <- tryCatch(
    data.table::fread(text = lines, colClasses = "character", header = TRUE,
                      sep = ",", fill = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw) || ncol(raw) != width) return(NULL)
  undo <- function(x) gsub(mark, ",", x, fixed = TRUE, useBytes = TRUE)
  for (j in seq_along(raw)) data.table::set(raw, j = j, value = undo(raw[[j]]))
  data.table::setnames(raw, undo(names(raw)))
  raw
}
