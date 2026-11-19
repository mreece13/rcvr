# unneeded columns to drop from the data
rcvr_DROP_COLS <- c(
  "Cast Vote Record", "RowNumber", "CvrNumber", "CVRNumber", "Cvr Number", "Cast.Vote.Record",
  "BallotID", "Ballot Id", "Dummy Row Number", "Row Number", "Unique Voting Identifier", 
  "Ballot Style", "BallotStyleID", "ballot_style2", "Ballot Style Id", "Ballot.Style", 
  "Ballot Type", "BallotType", "BallotTypeId",
  "Ballot Status", 
  "BoxID", "Box Id", "BoxPosition", "Box Position", 
  "PrecinctStyleName", "PrecinctStyleName (Redacted to Protect Voter Privacy)",
  "CountingGroup", "Counting Group",
  "TabulatorNum", "Tabulator Num", "Tabulator Name", "TabulatorId", 
  "Batch Id", "BatchId", "Batch", 
  "Record Id", "RecordId", 
  "ImprintedId", "Imprinted Id",
  "Image Path", 
  "Session Type", 
  "Voter Flag", 
  "Modified", 
  "Card Info", 
  "Pdf Name", 
  "ScanComputerName", "Scan Computer Name",
  "Status", 
  "Remade",
  "Voting Session Identifier",
  "Is Current",
  # Dominion CSV exports with the 4-row header spell these without spaces
  "ImagePath", "SessionType", "VoterFlag", "CardInfo", "PdfName",
  "UniqueVotingIdentifier", "VotingSessionIdentifier", "IsCurrent"
)

# appended by `header_processor()` to make duplicate column names unique.
# Chosen so it cannot occur in vendor data and can be stripped unambiguously
# before the name is split back into (contest, candidate, party).
rcvr_DUP_SENTINEL <- "__RCVRDUP__"

# an unnamed column, as the delimited readers name it: `data.table::fread()`
# falls back to the column index (`V15`), `readxl`'s "unique_quiet" repair to
# `...15`. One pattern serves every delim reader.
rcvr_PLACEHOLDER_RE <- "^(V\\d+|\\.\\.\\.\\d+)$"

#' Resolve runs of unnamed or repeated columns into one contest each
#'
#' In a delimited CVR export, a named contest column followed by a run of
#' unnamed columns is one contest spread over several columns. For a plain
#' export (one column per contest) that means a multi-seat race: each column
#' holds one of the voter's N selections, so the contest's magnitude is the
#' length of the run plus one. Some exports instead repeat the contest name
#' over every seat column (Outagamie WI, Hudson NJ, Lee FL); an adjacent run
#' of identical names is treated the same way.
#'
#' The same name can also recur *non-adjacently* (Hunterdon NJ regional
#' boards, one column per constituent town; Dane WI alder districts, one per
#' city; Outagamie's one-seat and three-seat Combined Locks trustee races).
#' Those are sometimes one contest and sometimes different contests, and the
#' header cannot tell which, so each later occurrence becomes its own contest,
#' `"<name> [2]"`, `"<name> [3]"`, ... in column order. The metadata store can
#' map them to the same office and district where they are one contest.
#'
#' A run with no named predecessor -- it starts at column 1, or the column
#' before it is a key/metadata column rather than a contest -- is left alone
#' and warned about. There is no name to inherit, so guessing one would invent
#' a contest.
#'
#' NB: this assumes the repeated columns are *seats*, not *ranks*. A
#' ranked-choice delimited export would also repeat columns and this rule
#' would inflate its magnitude; no delim-format contest in the corpus is RCV,
#' so no guard is applied. If `rcvr` later ingests RCV delim exports, this
#' needs a type check.
#'
#' @param nms Character vector of column names, in column order.
#' @param sentinel Append `rcvr_DUP_SENTINEL` and a run index to the inherited
#'   name (`TRUE`, the default), keeping every name unique so the columns
#'   survive to `pairs_from_delim()`, which strips the sentinel again. `FALSE`
#'   assigns the bare parent name, for callers that build the final name from
#'   further header rows and de-duplicate themselves. Repeated names are left
#'   as they are in that case.
#' @param warn Warn about unresolvable placeholder runs? `header_processor()`
#'   turns this off: its leading metadata columns are unnamed in the contest
#'   row by design and get their names from a later header row.
#'
#' @return A list with `names` (the corrected vector) and `magnitude`, a named
#'   integer vector of `contest -> magnitude` for the contests that were
#'   extended. `magnitude` is always empty when `sentinel = FALSE`.
resolve_placeholder_cols <- function(nms, sentinel = TRUE, warn = TRUE) {
  out <- list(names = nms, magnitude = integer(0))

  ind <- which(stringr::str_detect(nms, rcvr_PLACEHOLDER_RE))
  if (length(ind) == 0 && !anyDuplicated(nms)) {
    return(out)
  }

  # a placeholder run must not inherit the name of a key or metadata column:
  # `CvrNumber` followed by a blank junk column is not a two-seat race
  not_a_contest <- c(
    rcvr_DROP_COLS, unname(rcvr_RENAME_COLS), unique(names(rcvr_RENAME_COLS))
  )

  # `base` is the name each column continues: its own, or for a resolvable
  # placeholder its parent's
  base <- nms
  unresolved <- integer(0)

  for (r in if (length(ind) > 0) find_consecutive_ranges(ind) else list()) {
    parent_i <- r[1] - 1
    if (parent_i < 1 || parent_i %in% ind || nms[parent_i] %in% not_a_contest) {
      unresolved <- c(unresolved, r)
      next
    }
    base[r] <- nms[parent_i]
  }

  if (!sentinel) {
    out$names <- base
  } else {
    runs <- rle(base)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    for (k in seq_along(runs$values)) {
      v <- runs$values[k]
      pos <- starts[k]:ends[k]
      if (pos[1] %in% unresolved) next
      occ <- sum(runs$values[seq_len(k)] == v)

      if (v %in% not_a_contest) {
        # a repeated key column: keep the first, make later copies unique so
        # the frame stays valid; pairs_from_delim() drops them all by name
        if (occ > 1) {
          out$names[pos] <- paste0(v, rcvr_DUP_SENTINEL, seq_along(pos) + 100L * occ)
        } else if (length(pos) > 1) {
          out$names[pos[-1]] <- paste0(v, rcvr_DUP_SENTINEL, seq_along(pos[-1]))
        }
        next
      }

      contest <- if (occ == 1L) v else occurrence_name(v, occ)
      out$names[pos[1]] <- contest
      if (length(pos) > 1) {
        out$names[pos[-1]] <- paste0(contest, rcvr_DUP_SENTINEL, seq_along(pos[-1]))
        # the run length is an exact column count, not an inference. A
        # `contest||candidate` name repeated is not a seat count.
        if (!grepl("||", v, fixed = TRUE)) {
          out$magnitude[contest] <- length(pos)
        }
      }
    }
  }

  if (length(unresolved) > 0 && warn) {
    bad <- nms[unresolved]
    cli::cli_warn(
      c(
        "{length(bad)} unnamed column{?s} {?has/have} no named contest to continue.",
        "x" = "{.field {bad}}",
        "i" = "They are left as-is and will appear in the metadata as contests of their own."
      ),
      class = "rcvr_unresolved_placeholder_cols"
    )
  }

  out
}

# the name of the `occ`-th non-adjacent occurrence of a repeated column name.
# The marker goes on the contest part, before any `||candidate||party`.
occurrence_name <- function(v, occ) {
  tag <- paste0(" [", occ, "]")
  if (grepl("||", v, fixed = TRUE)) sub("||", paste0(tag, "||"), v, fixed = TRUE) else paste0(v, tag)
}

# various permutations of a label that the cell is redacted
rcvr_REDACT_NAMES = c("X", "redacted for voter privacy", "REDACTED", "Redacted", "[REDACTED]", "*", "redacted", "Redacted per 24-27-205.5 (4)(b)(III) C.R.S.")

# various permutations of the precinct column name
rcvr_RENAME_COLS <- c(
  ballot_style = "Ballot Style",
  ballot_style = "BallotStyle",
  ballot_style = "BallotStyleID",
  ballot_style = "BallotType",
  precinct = "Precinct Portion",
  precinct = "PrecinctPortionID",
  precinct = "Precinct",
  precinct = "PrecinctID",
  precinct = "Precinct Id",
  precinct = "PrecinctId",
  precinct = "Precinct Portion Id",
  precinct = "PrecinctBySplit",
  precinct = "PrecinctPortionId",
  precinct = "Precinct ID",
  precinct = "precinct_number",
  precinct = "PRECINCT CODE",
  precinct = "PRECINCT NAME",
  precinct = "PrecinctPortion",
  precinct = "PrecinctStyleName",
  # Ocean NJ 2024 (same export family as Passaic's "Precinct" column)
  precinct = "District"
)

#' Check if delimited file has an unconventional header
#'
#' @param path the path to the delimited file
#'
#' @return boolean
is_header <- function(path) {

  # get the top-left cell's contents
  if (stringr::str_detect(path, "csv$|CSV$")) {
    rlang::check_installed("data.table", reason = "to read CSV files")
    # sep fixed: a one-field title line ("Burlington General Election 2024")
    # makes fread guess a space separator
    d <- data.table::fread(path, nrows=1, select=1, header=FALSE, sep=",")[[1]]
  } else if (stringr::str_detect(path, "xls$|xlsx$|XLS$|XLSX$")) {
    rlang::check_installed("readxl", reason = "to read Excel files")
    d <- readxl::read_excel(path, range = "A1", col_names = FALSE, .name_repair = "unique_quiet") |> dplyr::pull()
  }

  is_header_cell(d)
}

# if we detect the top left cell to include something like this, it's almost certainly
# a header file. There may be a more systematic way to check for headers but this
# works without fail on every file in the data
is_header_cell <- function(d) {
  isTRUE(stringr::str_detect(d, stringr::regex("202|General|November", ignore_case = TRUE)))
}

#' Process delimited files with unconventional headers. They are
#' generated by one of the voting machine vendors, and include
#' the name of the election in the top left, then a blank line, then
#' three rows of information on each column. Sometimes the order of the rows varies
#' but they always include the contest, the name of the candidate, and their party.
#' In addition, the first m columns are metadata columns related to the CVR. This
#' function processes these files into a more regular delimited format, with a singular
#' row for the header. There is also an edge case in some counties (e.g., San Diego, CA 2020)
#' wherein the header is inexplicably repeated partway through the file. The chunk that
#' generates `bad_rows` removes this extra information.
#'
#' @param path The path to the file
#' @param n The number of rows to read. This is used to generate the raw contests
#' quickly, without reading the entire file.
#'
#' @return A tibble formatted with conventional headers
header_processor <- function(path, n = Inf, grid = NULL) {

  checkmate::assertCharacter(path)
  checkmate::assert_number(n, lower=1)

  if (!is.null(grid)) {
    # an .xlsx already read by read_xlsx_grid(): the title row is grid row 1
    # and the contest row is grid row 2, as `skip = 1, header = TRUE` reads it
    df <- grid_with_header(grid, 2L)
    if (is.finite(n)) df <- df[seq_len(min(n, nrow(df)))]
  } else if (stringr::str_detect(path, "csv$|CSV$")) {
    rlang::check_installed("data.table", reason = "to read CSV files")
    df <- data.table::fread(
      path,
      skip=1,
      nrows=n,
      header=TRUE,
      # `character()` (a zero-length vector) is a no-op for `colClasses`, which
      # let fread's type inference silently turn all-numeric-looking metadata
      # columns (e.g. a Precinct column of "001"/"002") into integers,
      # dropping the leading zero. The pairs-frame contract requires
      # character; force it here as done in `read_delim_cvr()`.
      colClasses="character"
    )

  } else if (stringr::str_detect(path, "xls$|xlsx$|XLS$|XLSX$")) {
    rlang::check_installed("readxl", reason = "to read Excel files")

    df <- readxl::read_excel(
      path,
      skip=1,
      n_max=n,
      .name_repair = "unique_quiet",
      col_names = TRUE,
      col_types = "text",
    )
  }

  # A blank contest cell in this format means "same contest as the column to
  # the left, next candidate" -- the vendor writes the contest name once over
  # a merged range of candidate columns. Resolve those names before the paste
  # below, or the "^V\\d+" strip blanks them and the candidate name ends up in
  # the contest position. `sentinel = FALSE` because the pasted name already
  # carries the candidate and party, make.unique() de-duplicates what is left,
  # and a sentinel inserted here would sit mid-string where
  # `pairs_from_delim()` cannot strip it. No magnitude is inferred: a run here
  # is extra candidates, not extra seats.
  # Pueblo CO's 2022 recount writes the three rows in another order: key
  # names with each candidate's party, then contests, then candidates. Its
  # contests sit in the first data row instead of the names (Dominion contest
  # names end in "(Vote For=N)"); rotate the rows into the usual order. The
  # party row has blanks for no party, which are not merged ranges, so the
  # placeholder fill below must not touch it.
  # Some Dominion exports (Mercer NJ 2022) write the software version
  # ("5.15.16.1") in the first cell of the contest row, which would otherwise
  # become the contest of every metadata column as a merged range.
  if (grepl("^\\d+(\\.\\d+)+$", colnames(df)[1])) colnames(df)[1] <- "V1"
  vote_for <- function(x) sum(stringr::str_detect(x, stringr::fixed("(Vote For=")), na.rm = TRUE)
  first <- vapply(df, function(x) as.character(x[1]), character(1), USE.NAMES = FALSE)
  rotated <- nrow(df) >= 2 && vote_for(colnames(df)) == 0 && vote_for(first) > 0
  if (rotated) {
    df <- data.table::as.data.table(df)
    second <-vapply(df, function(x) as.character(x[2]), character(1), USE.NAMES = FALSE)
    party <- colnames(df)
    is_contest <- !is.na(first) & first != ""
    party[stringr::str_detect(party, rcvr_PLACEHOLDER_RE) | party == "" | !is_contest] <- NA
    contest <- ifelse(is_contest, first, colnames(df))
    contest <- resolve_placeholder_cols(contest, sentinel = FALSE, warn = FALSE)$names
    data.table::set(df, i = 1L, j = seq_along(df), value = as.list(second))
    data.table::set(df, i = 2L, j = seq_along(df), value = as.list(party))
    colnames(df) <- contest
  } else {
    colnames(df) <- resolve_placeholder_cols(colnames(df), sentinel = FALSE, warn = FALSE)$names
  }

  ncols = ncol(df)

  # sometimes these header files have repeated rows in the middle that need to be removed
  # `[[`: on a data.table (every CSV), `df[j]` is row j, not column j
  bad_rows = which(df[[ncols-1]] == colnames(df)[ncols-1] |> stringr::str_remove("\\.\\.\\.\\d+")) |>
    # the block is title, contest, candidate, party rows (rotated: title,
    # party, contest, candidate), and the match is on the contest row
    purrr::map(~ (.x - 1 - rotated):(.x + 2 - rotated)) |>
    unlist()

  colnames(df) = paste(colnames(df), df[1, ], df[2, ], sep = "||") |>
    stringr::str_remove("\\.\\.\\.\\d+") |>
    # "NA" only as a whole component, or "||NATHAN HOCHMAN" loses its "NA"
    stringr::str_remove_all("^NA\\|\\||\\|\\|NA(?=\\|\\||$)|^\\|\\|") |>
    stringr::str_remove_all("\\|\\|NA(?=\\|\\||$)|^\\|\\|") |>
    iconv(to = "UTF-8", sub = "") |>
    stringr::str_remove_all("^V\\d+") |>
    stringr::str_remove_all("^\\|\\|") |>
    stringr::str_remove_all("^\\|\\|") |>
    # a metadata column (e.g. CvrNumber, Precinct) with blank candidate/party
    # header cells pastes as e.g. "CvrNumber||||"; fread reads a blank cell as
    # "" (not NA) once colClasses forces character, so the "||NA" removals
    # above don't catch it. A key column must never carry this artefact, so
    # strip trailing empty "||" components here too.
    stringr::str_remove_all("(\\|\\|)+$") |>
    make.unique(sep = rcvr_DUP_SENTINEL)

  df[-c(bad_rows, 1, 2), ]

}

# Pairs the metadata store never seeds a row for. gen_metadata() drops
# undervote/overvote/write-in/"0"/redacted from the seed after cleaning
# raw_candidate via seed_candidate(), so join_metadata()'s anti-join check
# must exempt the same raw pairs or it would abort a county on perfectly good
# ballots (a write-in vote, an overvote, a redacted cell). One regex serves
# both call sites so they cannot drift: "undervote"/"overvote"/"0" are
# spelled identically raw and cleaned; "Write" is the same substring test
# seed_candidate() uses to collapse a raw write-in spelling to the cleaned
# "WI"; and the literal "WI" is included so gen_metadata()'s post-seed filter
# (which tests the already-cleaned `candidate` column) keeps its exact prior
# behaviour. "redacted" is the literal sentinel pairs_from_delim() maps every
# rcvr_REDACT_NAMES spelling to (R/read_delim.R), so a redacted cell never
# has to survive as an NA raw_candidate, which join_metadata() would coalesce
# to non-exempt and abort on.
rcvr_UNSEEDED_RE <- stringr::regex("^undervote$|^overvote$|^WI$|^0$|^redacted$|Write", ignore_case = TRUE)

# the column contract every reader must satisfy
rcvr_PAIRS_COLS <- c(
  cvr_id = "integer",
  precinct = "character",
  contest = "character",
  raw_candidate = "character",
  raw_party = "character",
  rank = "integer"
)

#' Assert that a reader returned a valid pairs frame
#'
#' @param pairs A tibble returned by one of the `read_*_cvr()` readers.
#' @param call The calling environment, for error reporting.
#'
#' @return `pairs`, invisibly. Aborts with class `rcvr_bad_pairs` otherwise.
assert_pairs <- function(pairs, call = rlang::caller_env()) {
  # all(is.na(x)) is vacuously TRUE for a length-0 vector, so every type
  # check below is satisfied by an empty frame. A reader whose filters
  # removed every row would otherwise return a zero-row "success",
  # indistinguishable from a clean_cvr() run that silently lost a whole
  # county.
  if (nrow(pairs) == 0) {
    cli::cli_abort(
      "Pairs frame has zero rows.",
      class = "rcvr_bad_pairs",
      call = call
    )
  }

  # a reader that cannot find the contests in a layout it half-recognises
  # (Hart's nested XML before it was supported) yields one NA-contest row per
  # ballot; that passes every type check but carries no votes
  if ("contest" %in% colnames(pairs) && all(is.na(pairs$contest))) {
    cli::cli_abort(
      "Every row of the pairs frame has an NA {.field contest}; the reader found no contests.",
      class = "rcvr_bad_pairs",
      call = call
    )
  }

  missing <- setdiff(names(rcvr_PAIRS_COLS), colnames(pairs))
  if (length(missing) > 0) {
    cli::cli_abort(
      "Pairs frame is missing required column{?s}: {.field {missing}}",
      class = "rcvr_bad_pairs",
      call = call
    )
  }

  actual <- vapply(pairs[names(rcvr_PAIRS_COLS)], typeof, character(1))
  expected <- unname(rcvr_PAIRS_COLS)
  # R stores integers as "integer" and characters as "character"; logical NA
  # columns are tolerated only when the column is entirely NA
  bad <- names(rcvr_PAIRS_COLS)[
    actual != expected & !vapply(pairs[names(rcvr_PAIRS_COLS)], function(x) all(is.na(x)), logical(1))
  ]
  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "Pairs frame column{?s} {.field {bad}} {?has/have} the wrong type.",
        "i" = "Expected {.val {unname(rcvr_PAIRS_COLS[bad])}}, got {.val {unname(actual[bad])}}."
      ),
      class = "rcvr_bad_pairs",
      call = call
    )
  }

  invisible(pairs)
}

#' Re-encode names that are not valid UTF-8 from Windows-1252
#'
#' Some exports are written in Windows-1252, not UTF-8 (Bergen NJ `YES/S\xcd`,
#' Collin TX curly quotes, Fort Bend TX en dashes, DC accented names). Read
#' as bytes, those names are not valid UTF-8: stringr aborts on them, and the
#' metadata sheet cannot hold them, so a store row could never match its own
#' pair. Every string that is not valid UTF-8 is converted from Windows-1252;
#' the five bytes that Windows-1252 leaves undefined fall back to Latin-1.
#' Valid UTF-8 is left untouched.
#'
#' @param pairs A pairs frame.
#'
#' @return `pairs`, attributes kept.
#' @keywords internal
fix_encoding <- function(pairs) {
  for (v in intersect(c("precinct", "contest", "raw_candidate", "raw_party"), names(pairs))) {
    x <- pairs[[v]]
    if (!is.character(x)) next
    bad <- which(!is.na(x) & !validUTF8(x))
    if (length(bad) == 0) next
    out <- iconv(x[bad], from = "windows-1252", to = "UTF-8")
    miss <- is.na(out)
    out[miss] <- iconv(x[bad][miss], from = "latin1", to = "UTF-8")
    x[bad] <- out
    pairs[[v]] <- x
  }
  pairs
}

#' Collapse CSV-escaped quotes left doubled by the reader
#'
#' A CSV field with a quote in it is written quoted, the inner quote doubled
#' (RFC 4180): `"DEM Anthony ""Tony"" Eldon"`. data.table 1.18.4's `fread()`
#' strips the outer quotes but returns the inner pair as written,
#' `DEM Anthony ""Tony"" Eldon`, so the same candidate keys differently from
#' an Excel or JSON export of it (Manatee FL 2020). A literal `""` inside a
#' name is not plausible, so every `""` in the pairs frame's string columns is
#' collapsed to `"`.
#'
#' @param pairs A pairs frame.
#'
#' @return `pairs`, attributes kept.
#' @keywords internal
undouble_quotes <- function(pairs) {
  for (v in intersect(c("precinct", "contest", "raw_candidate", "raw_party"), names(pairs))) {
    x <- pairs[[v]]
    if (!is.character(x)) next
    # read_cvr() runs fix_encoding() first; anything still not valid UTF-8
    # is skipped, because stringr aborts on it
    ok <- !is.na(x) & validUTF8(x)
    hit <- which(ok)[stringr::str_detect(x[ok], stringr::fixed('""'))]
    if (length(hit) == 0) next
    x[hit] <- stringr::str_replace_all(x[hit], stringr::fixed('""'), '"')
    pairs[[v]] <- x
  }
  pairs
}

#' Trim edge whitespace from the names that key the metadata store
#'
#' A raw name with a leading or trailing space (`"Tammy Duckworth "`,
#' Passaic NJ 2020) cannot round-trip through the metadata sheet, which
#' strips edge whitespace when rows are appended, so its pair would never
#' match its own store row and the county would stay pending. Interior
#' whitespace is left alone.
#'
#' @param pairs A pairs frame.
#'
#' @return `pairs`, attributes kept.
#' @keywords internal
trim_names <- function(pairs) {
  for (v in intersect(c("contest", "raw_candidate", "raw_party"), names(pairs))) {
    x <- pairs[[v]]
    if (!is.character(x)) next
    # invalid UTF-8 is left as read, as in undouble_quotes()
    ok <- !is.na(x) & validUTF8(x)
    hit <- which(ok)[x[ok] != stringr::str_trim(x[ok])]
    if (length(hit) == 0) next
    x[hit] <- stringr::str_trim(x[hit])
    x[hit][x[hit] == ""] <- NA_character_
    pairs[[v]] <- x
  }
  pairs
}
