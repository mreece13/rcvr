#' Merge ballots that a vendor split across several rows (multi-card ballots)
#'
#' Some exports write one row per ballot *card*: a two-card ballot is two
#' consecutive rows with the same ballot style, the first holding the card-1
#' contests and the second the card-2 contests. A contest that is on a card
#' but not voted is written as `"0"`; a contest that is not on that card is
#' blank. So within one ballot style, card rows have complementary coverage.
#'
#' Detection is per ballot style (with any `" [n]"` card suffix stripped):
#' take the style's most-filled contest column as the anchor; columns filled
#' (almost) only on anchor rows are card-1 columns, columns filled (almost)
#' only on non-anchor rows are continuation columns. A row with nothing in
#' the card-1 columns and something in the continuation columns is a
#' continuation and is merged into the row just above it, provided that row
#' has the same style. Ballot styles whose contests simply differ (one
#' party's primary, a city's local races) never split *within* a style, so
#' they are left alone.
#'
#' @param raw A wide CVR tibble.
#' @param path The source path, used only in the warning message.
#'
#' @return `raw`, with continuation rows merged into their ballot.
fix_fragmentation <- function(raw, path = NA) {
  if (!("ballot_style" %in% colnames(raw)) || nrow(raw) < 2) {
    return(raw)
  }

  key <- c(rcvr_DROP_COLS, unname(rcvr_RENAME_COLS), names(rcvr_RENAME_COLS),
           "cvr_id", "ballot_style2")
  contest_cols <- setdiff(colnames(raw), key)
  if (length(contest_cols) < 2) return(raw)

  filled <- vapply(raw[contest_cols], function(x) !is.na(x) & x != "",
                   logical(nrow(raw)))
  if (is.null(dim(filled))) filled <- matrix(filled, nrow = 1)
  style <- stringr::str_remove(as.character(raw$ballot_style), " \\[\\d+\\]$")
  style[is.na(style)] <- ""

  cont <- logical(nrow(raw))
  for (s in unique(style)) {
    rows <- which(style == s)
    if (length(rows) < 2) next
    f <- filled[rows, , drop = FALSE]
    # card-1 contests come first in column order, so the first contest used
    # by this style is on card 1 (the most-filled column is not: card-1 and
    # card-2 columns are each filled on about half the rows)
    anchor <- which(colSums(f) > 0)[1]
    if (is.na(anchor)) next
    a <- f[, anchor]
    if (all(a) || !any(a)) next
    rate_a <- colMeans(f[a, , drop = FALSE])
    rate_b <- colMeans(f[!a, , drop = FALSE])
    card1 <- rate_a > 0 & rate_b < 0.01
    card2 <- rate_b > 0 & rate_a < 0.01
    if (!any(card2)) next
    # the non-anchor rows must be (almost) entirely continuation cards; a
    # style where they mostly carry card-1 contests is not multi-card
    fills_b <- sum(f[!a, , drop = FALSE])
    if (fills_b == 0 || sum(f[!a, card2, drop = FALSE]) / fills_b < 0.9) next
    is_cont <- rowSums(f[, card1, drop = FALSE]) == 0 & rowSums(f[, card2, drop = FALSE]) > 0
    cont[rows[is_cont]] <- TRUE
  }

  if (!any(cont)) return(raw)

  # A continuation card can only be linked to its voter when the export keeps
  # each voter's cards together: card 2 directly after card 1, same style.
  # Election-day tabulators often take cards in arbitrary order (Clark OH
  # 2024), and then no linkage exists in the data at all -- merging there
  # would attach cards to the wrong voters. So link batch by batch, and only
  # in batches where (almost) every continuation follows a main row of the
  # same style.
  batch_cols <- intersect(c("TabulatorNum", "Tabulator Num", "TabulatorId",
                            "BatchId", "Batch Id", "Batch"), colnames(raw))
  batch <- if (length(batch_cols) > 0) {
    do.call(paste, c(lapply(raw[batch_cols], as.character), sep = "\r"))
  } else {
    rep("", nrow(raw))
  }

  link <- function(o) {
    s <- style[o]
    k <- cont[o]
    b <- batch[o]
    prev_ok <- c(FALSE, s[-1] == s[-length(s)] & !k[-length(k)])
    share_ok <- tapply(prev_ok[k], b[k], mean)
    linked_batch <- names(share_ok)[share_ok >= 0.98]
    list(
      o = o,
      linked = k & prev_ok & b %in% linked_batch,
      unlinked = k & !(b %in% linked_batch)
    )
  }
  res <- link(seq_len(nrow(raw)))

  # Some exports are sorted by precinct rather than scan order (Pueblo CO
  # 2024), so a voter's cards are not adjacent rows even though they were
  # scanned back to back. Scan order survives in the record id within each
  # tabulator batch; when linking in file order leaves cards unlinked, try
  # that order too and keep whichever links more. The same >= 98% per-batch
  # rule applies, so a batch whose cards were genuinely shuffled still stays
  # unlinked.
  rec_col <- intersect(c("RecordId", "Record Id"), colnames(raw))
  if (any(res$unlinked) && length(rec_col) > 0 && length(batch_cols) > 0) {
    rec <- suppressWarnings(as.numeric(raw[[rec_col[1]]]))
    if (!anyNA(rec)) {
      alt <- link(order(batch, rec))
      if (sum(alt$linked) > sum(res$linked)) res <- alt
    }
  }

  if (!identical(res$o, seq_len(nrow(raw)))) {
    raw <- raw[res$o, , drop = FALSE]
    filled <- filled[res$o, , drop = FALSE]
  }
  unlinked <- res$unlinked
  cont <- res$linked

  # cli's {.file} errors on NA, and `path` is NA when called without one
  src <- if (length(path) == 1 && !is.na(path)) as.character(path) else "CVR"

  if (any(unlinked)) {
    cli::cli_warn(
      c("{.file {src}}: {sum(unlinked)} continuation card row{?s} cannot be linked to a voter (cards out of order within their batch); each is kept as its own ballot row.",
        "i" = "For these, {.field cvr_id} identifies a card, not a voter."),
      class = "rcvr_unlinked_cards"
    )
  }
  if (!any(cont)) return(raw)

  cli::cli_warn(
    "{.file {src}}: merged {sum(cont)} continuation card row{?s} into the ballot row above (multi-card ballots)."
  )

  grp <- cumsum(!cont)
  main_row <- which(!cont)[grp]
  for (col in contest_cols) {
    v <- raw[[col]]
    take <- cont & filled[, col]
    if (any(take)) {
      tgt <- main_row[take]
      blank_tgt <- is.na(v[tgt]) | v[tgt] == ""
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
#'
#' @return A wide tibble, one row per ballot, with `precinct` and
#'   `ballot_style` present (possibly all-`NA`).
#' @export
read_delim_cvr <- function(path) {
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
    is_header_cell(grid[[1]][1])
  }
  if (header) {
    raw <- header_processor(path, grid = grid)
  } else if (!is.null(grid)) {
    raw <- grid_with_header(grid, 1L)
  } else {
    # names are read as written (fread keeps duplicates; readxl "minimal"
    # does not append `...NN` to them): a repeated contest name is resolved
    # by resolve_placeholder_cols() below, never by a column-index suffix
    raw <- switch(ext,
      "CSV" = data.table::fread(path, colClasses = "character", header = TRUE),
      "XLS" = , "XLSX" = readxl::read_excel(path, col_types = "text", .name_repair = "minimal")
    )
    blank <- is.na(colnames(raw)) | colnames(raw) == ""
    colnames(raw)[blank] <- paste0("...", which(blank))
  }
  raw <- data.table::as.data.table(raw)

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
    } else {
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
  raw
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
  # the reader recorded one magnitude per multi-column contest; carry it
  # across by hand onto the returned pairs frame, where gen_metadata() picks
  # it up
  magnitude <- attr(raw, "rcvr_magnitude")

  raw <- fix_fragmentation(raw, path)
  n <- nrow(raw)
  precinct <- if ("precinct" %in% colnames(raw)) {
    as.character(raw[["precinct"]])
  } else {
    rep(NA_character_, n)
  }

  # a dedup sentinel never changes what a column is: a repeated key column is
  # still a key column, and a seat column still belongs to its contest
  bare <- stringr::str_remove(colnames(raw), paste0(rcvr_DUP_SENTINEL, "\\d+$"))
  is_value <- !(bare %in% c(rcvr_DROP_COLS, "ballot_style", "ballot_style2", "precinct", "cvr_id"))
  value_cols <- which(is_value)
  info <- parse_contest_cols(bare[value_cols])

  # Collect only the cells that carry a vote, column by column. Pivoting
  # every cell to long form first (as tidyr::pivot_longer() did) costs
  # rows x columns long rows, nearly all of them "0" or blank, before any
  # filter can drop them: that was the 10-180 GB peak on wide exports.
  has_zero <- logical(n)
  ids <- vector("list", length(value_cols))
  vals <- vector("list", length(value_cols))
  for (k in seq_along(value_cols)) {
    x <- raw[[value_cols[k]]]
    if (!is.character(x)) x <- as.character(x)
    zero <- which(x == "0")
    has_zero[zero] <- TRUE
    keep <- which(!is.na(x) & x != "" & x != "0")
    ids[[k]] <- keep
    vals[[k]] <- x[keep]
  }
  lens <- lengths(ids)
  cvr_id <- unlist(ids, use.names = FALSE)
  value <- unlist(vals, use.names = FALSE)
  col <- rep.int(seq_along(value_cols), lens)
  rm(ids, vals)
  if (is.null(cvr_id)) {
    cvr_id <- integer(0); value <- character(0); col <- integer(0)
  }

  # some rows are aggregated counts (CvrNumber = `Redacted and Aggregated...`),
  # and some exports repeat the header partway through (a cell equal to its
  # own contest's name); neither is a vote
  u <- unique(value)
  num <- suppressWarnings(as.numeric(u))
  aggregated <- u[!is.na(num) & num > 1]
  junk <- value %in% aggregated | value == info$contest[col]
  junk[is.na(junk)] <- FALSE
  junk_rows <- unique(cvr_id[junk])
  cvr_id <- cvr_id[!junk]; value <- value[!junk]; col <- col[!junk]

  # if they voted, use the column's candidate (candidate-per-column layout);
  # otherwise the cell is the candidate (contest-per-column layout)
  cand <- info$candidate[col]
  raw_candidate <- dplyr::case_when(
    value %in% rcvr_REDACT_NAMES ~ "redacted",
    .default = dplyr::coalesce(cand, value)
  )

  votes <- data.table::data.table(
    cvr_id = cvr_id,
    precinct = precinct[cvr_id],
    contest = info$contest[col],
    raw_candidate = raw_candidate,
    raw_party = info$party[col],
    rank = info$rank[col],
    ord = col
  )

  # A ballot gets an undervote for each contest seen in its precinct that it
  # cast no vote in. Every ballot row counts, including one with no marks at
  # all (Georgia, Sauk WI) -- unless the row only held aggregated counts or a
  # repeated header, which are not ballots, or the row is entirely empty.
  # A row is a ballot if it carries an id or other key cell, or a "0".
  key_cols <- which(!is_value & !(bare %in% c("precinct", "ballot_style", "ballot_style2")))
  has_key <- logical(n)
  for (j in key_cols) {
    x <- raw[[j]]
    has_key <- has_key | (!is.na(x) & as.character(x) != "")
  }
  is_ballot <- has_key | has_zero
  is_ballot[setdiff(junk_rows, cvr_id)] <- FALSE
  is_ballot[unique(cvr_id)] <- TRUE

  ballots <- data.table::data.table(cvr_id = which(is_ballot), precinct = precinct[is_ballot])
  first_ord <- votes[, list(ord = min(ord)), by = "contest"]
  pc <- unique(votes[, c("precinct", "contest")])
  grid <- ballots[pc, on = "precinct", allow.cartesian = TRUE, nomatch = NULL]
  voted <- unique(votes[, c("cvr_id", "contest")])
  under <- grid[!voted, on = c("cvr_id", "contest")]
  under <- first_ord[under, on = "contest"]
  under[, `:=`(raw_candidate = "undervote", raw_party = NA_character_, rank = NA_integer_)]

  pairs <- data.table::rbindlist(list(votes, under), use.names = TRUE)
  data.table::setorderv(pairs, c("cvr_id", "ord"))
  pairs[, ord := NULL]
  for (v in c("contest", "raw_candidate", "raw_party")) {
    data.table::set(pairs, which(pairs[[v]] == ""), v, NA_character_)
  }
  pairs[, cvr_id := as.integer(cvr_id)]

  pairs <- tibble::as_tibble(pairs)
  attr(pairs, "rcvr_magnitude") <- magnitude

  assert_pairs(pairs)
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
