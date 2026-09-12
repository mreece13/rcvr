# key a special reader on (state, county)
special_key <- function(state, county) {
  paste(stringr::str_to_upper(state), stringr::str_to_upper(county), sep = "|")
}

# fill in the contests a ballot skipped, so every ballot has a row for every
# contest that appeared anywhere in the file. Shared by the special readers,
# which mostly arrive as one row per cast mark.
complete_undervotes <- function(long) {
  long |>
    tidyr::complete(cvr_id, tidyr::nesting(contest)) |>
    dplyr::mutate(
      raw_candidate = tidyr::replace_na(raw_candidate, "undervote")
    ) |>
    dplyr::group_by(cvr_id) |>
    tidyr::fill(precinct, .direction = "downup") |>
    dplyr::ungroup()
}

# NEW JERSEY | CUMBERLAND
# one row per (ballot, contest, candidate) with a boolean vote flag; no
# precinct column. Ported from cvrs code/functions.R:511-522.
read_special_nj_cumberland <- function(path) {
  files <- if (fs::is_dir(path)) {
    list.files(path, pattern = "\\.csv$", full.names = TRUE)
  } else {
    path
  }

  lapply(files, function(f) {
    data.table::fread(f, colClasses = "character", header = TRUE) |>
      tibble::as_tibble()
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      is_vote = stringr::str_to_lower(IsVote) %in% c("true", "1", "yes")
    ) |>
    dplyr::filter(is_vote) |>
    dplyr::transmute(
      cvr_id = as.integer(CVRNumber),
      precinct = NA_character_,
      contest = Contest,
      raw_candidate = stringr::str_squish(Candidate)
    ) |>
    complete_undervotes() |>
    dplyr::mutate(
      raw_party = NA_character_,
      rank = NA_integer_
    ) |>
    dplyr::select(cvr_id, precinct, contest, raw_candidate, raw_party, rank)
}

# TEXAS | DENTON
# unique flat format: one row per mark, with its own column names.
# Ported from cvrs code/functions.R:415-425.
read_special_tx_denton <- function(path) {
  data.table::fread(
    path,
    colClasses = "character",
    header = TRUE,
    select = c("cid", "Precinct", "Race", "Candidate")
  ) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      cvr_id = as.integer(cid),
      precinct = Precinct,
      contest = Race,
      raw_candidate = stringr::str_squish(Candidate)
    ) |>
    dplyr::filter(raw_candidate != "0", raw_candidate != "") |>
    complete_undervotes() |>
    dplyr::mutate(
      raw_candidate = dplyr::if_else(
        stringr::str_detect(raw_candidate, stringr::regex("^undervote$", TRUE)),
        "undervote",
        raw_candidate
      ),
      raw_party = NA_character_,
      rank = NA_integer_
    ) |>
    dplyr::select(cvr_id, precinct, contest, raw_candidate, raw_party, rank)
}

# FLORIDA | WALTON
# a single wide CSV in the plain DELIM shape (contest||candidate||party
# columns, 0/1 cast-vote indicators). Ported from cvrs code/functions.R:520-526.
read_special_fl_walton <- function(path) {
  read_delim_cvr(path) |>
    pairs_from_delim(path)
}

# FLORIDA | LEE, MANATEE, MARION, SANTA ROSA, SARASOTA
# several .xls files per county, row-bound, in the same plain DELIM shape as
# Walton. Ported from cvrs code/functions.R:527-534. Identical across these
# five counties, so one function is registered under all five keys rather
# than duplicated.
read_special_fl_multi <- function(path) {
  read_delim_multi_cvr(path) |>
    pairs_from_delim(path)
}

# PENNSYLVANIA | ALLEGHENY
# many CSVs per election, row-bound, with cvrNumber/precinct/contest/candidate
# columns. Ported from cvrs code/functions.R:539-598 — note the source's
# hardcoded return path at :595 is a bug confined to that branch and is not
# ported here.
read_special_pa_allegheny <- function(path) {
  files <- if (fs::is_dir(path)) {
    # Filter by extension so a stray non-CSV file dropped into the county's
    # directory (a readme, a leftover .xlsx, a Dropbox conflict copy) doesn't
    # crash `fread` with a raw unclassed error, same as
    # `read_delim_multi_cvr()` in R/read_delim.R and
    # `read_special_nj_cumberland()` above.
    list.files(path, pattern = "\\.csv$", full.names = TRUE)
  } else {
    path
  }

  if (length(files) == 0) {
    cli::cli_abort(
      "No CSV files found in {.file {path}}.",
      class = "rcvr_no_files"
    )
  }

  lapply(files, function(f) {
    data.table::fread(f, colClasses = "character", header = TRUE) |>
      tibble::as_tibble()
  }) |>
    dplyr::bind_rows() |>
    dplyr::transmute(
      cvr_id = as.integer(cvrNumber),
      precinct = precinct,
      contest = contest,
      raw_candidate = stringr::str_squish(candidate)
    ) |>
    complete_undervotes() |>
    dplyr::mutate(
      raw_party = NA_character_,
      rank = NA_integer_
    ) |>
    dplyr::select(cvr_id, precinct, contest, raw_candidate, raw_party, rank)
}

# TEXAS | MONTGOMERY
# no ballot IDs in the source file; rows are organised sequentially, one row
# per (contest, candidate) mark, contest first. Ported from cvrs
# code/function_contests.R:98-101, which only ever reads this file to list
# distinct contests (`process_special()` has no reading branch for this
# county) — so the row shape below (contest, candidate) is inferred from
# that snippet's `col_select = 1, col_names = "contest"` call against a
# headerless file, which is all the source confirms.
#
# Design decision: reconstruct cvr_id from row order by treating a repeated
# contest name as a ballot boundary. Each ballot's rows list its contests in
# a fixed order without repeats (Montgomery is not an RCV county, so a
# contest appears at most once per ballot); the moment a contest we've
# already seen since the last boundary shows up again, that is the first row
# of the next ballot. This does not require every ballot to start with the
# same first contest (unlike keying off a fixed "first contest" value), so it
# tolerates a ballot that skips its first race. It does assume no contest is
# genuinely duplicated within one ballot.
assign_montgomery_cvr_id <- function(contest) {
  cvr_id <- integer(length(contest))
  seen <- character(0)
  current <- 1L

  for (i in seq_along(contest)) {
    if (contest[i] %in% seen) {
      current <- current + 1L
      seen <- character(0)
    }
    seen <- c(seen, contest[i])
    cvr_id[i] <- current
  }

  cvr_id
}

read_special_tx_montgomery <- function(path) {
  data.table::fread(
    path,
    colClasses = "character",
    header = FALSE,
    col.names = c("contest", "candidate")
  ) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      cvr_id = assign_montgomery_cvr_id(contest),
      precinct = NA_character_,
      contest = contest,
      raw_candidate = stringr::str_squish(candidate)
    ) |>
    complete_undervotes() |>
    dplyr::mutate(
      raw_party = NA_character_,
      rank = NA_integer_
    ) |>
    dplyr::select(cvr_id, precinct, contest, raw_candidate, raw_party, rank)
}

# CALIFORNIA | LOS ANGELES
# a separate code table (CandidateCodes.csv: code, candidate, contest) joined
# against a ballot file of cast marks. Ported from cvrs
# code/function_contests.R:70-78, which only ever reads CandidateCodes.csv to
# list distinct contests for manual classification (process_special() has no
# reading branch for this county, same gap as Montgomery) — so the ballot
# file's own row shape is not confirmed by any source in `cvrs`.
#
# Design decision: since CandidateCodes.csv supplies the contest for a code
# (the source's own read of that file selects `contest` as the third
# column), and the task brief describes the join as "code -> (candidate,
# contest)" rather than "code -> candidate", the ballot file is read as one
# row per cast mark with only a ballot id, a precinct, and a code column
# (`CVRNumber`, `PrecinctPortion`, `Code`) -- the contest itself comes from
# the code table, not from the ballot file. A ballot that omits a contest
# entirely (no code row for it) is completed to "undervote" the same way as
# every other one-row-per-mark special reader.
read_special_ca_los_angeles <- function(path) {
  dir <- if (fs::is_dir(path)) path else fs::path_dir(path)
  ballot_path <- if (fs::is_dir(path)) {
    # `list.files()` has no `perl=` argument (unlike `grepl()`), so a
    # negative-lookahead `pattern=` here would error the moment this branch
    # actually ran — list every .csv and drop the codes file by basename
    # instead.
    csvs <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
    csvs[basename(csvs) != "CandidateCodes.csv"]
  } else {
    path
  }
  codes_path <- fs::path(dir, "CandidateCodes.csv")

  codes <- data.table::fread(
    codes_path,
    colClasses = "character",
    header = FALSE,
    skip = 1,
    col.names = c("code", "candidate", "contest")
  ) |>
    tibble::as_tibble()

  ballots <- data.table::fread(
    ballot_path,
    colClasses = "character",
    header = TRUE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(cvr_id = CVRNumber, precinct = PrecinctPortion, code = Code)

  joined <- dplyr::left_join(ballots, codes, dplyr::join_by(code))

  unresolved <- dplyr::filter(joined, is.na(contest))
  if (nrow(unresolved) > 0) {
    ids <- unique(unresolved$code)
    cli::cli_abort(
      c(
        "{length(ids)} code{?s} in the CVR export could not be resolved against {.file CandidateCodes.csv}.",
        "i" = "Unresolved code{?s}: {.val {utils::head(ids, 10)}}"
      ),
      class = "rcvr_unresolved_ids",
      manifest = "CandidateCodes",
      ids = ids,
      unresolved = unresolved
    )
  }

  joined |>
    dplyr::transmute(
      cvr_id = as.integer(cvr_id),
      precinct = precinct,
      contest = contest,
      raw_candidate = stringr::str_squish(candidate)
    ) |>
    complete_undervotes() |>
    dplyr::mutate(
      raw_party = NA_character_,
      rank = NA_integer_
    ) |>
    dplyr::select(cvr_id, precinct, contest, raw_candidate, raw_party, rank)
}

#' Registered readers for counties whose CVR format fits no general parser
#'
#' Names are `"STATE|COUNTY"`, upper case. Each value is a function taking a
#' single `path` and returning a pairs frame. Adding a county means adding one
#' entry here and one test; nothing else in the package changes.
rcvr_SPECIAL_READERS <- list(
  "NEW JERSEY|CUMBERLAND" = read_special_nj_cumberland,
  "TEXAS|DENTON" = read_special_tx_denton,
  "FLORIDA|WALTON" = read_special_fl_walton,
  "FLORIDA|LEE" = read_special_fl_multi,
  "FLORIDA|MANATEE" = read_special_fl_multi,
  "FLORIDA|MARION" = read_special_fl_multi,
  "FLORIDA|SANTA ROSA" = read_special_fl_multi,
  "FLORIDA|SARASOTA" = read_special_fl_multi,
  "PENNSYLVANIA|ALLEGHENY" = read_special_pa_allegheny,
  "TEXAS|MONTGOMERY" = read_special_tx_montgomery,
  "CALIFORNIA|LOS ANGELES" = read_special_ca_los_angeles
)

#' Is a special reader registered for this county?
#'
#' @param state,county County identity, case-insensitive.
#'
#' @return `TRUE` or `FALSE`.
#' @export
has_special_reader <- function(state, county) {
  special_key(state, county) %in% names(rcvr_SPECIAL_READERS)
}

#' Read a county whose CVR format fits no general parser
#'
#' @param path Path to the county's CVR file or directory.
#' @param state,county County identity, case-insensitive.
#'
#' @return A pairs frame.
#' @export
read_special_cvr <- function(path, state, county) {
  key <- special_key(state, county)

  if (!(key %in% names(rcvr_SPECIAL_READERS))) {
    cli::cli_abort(
      c(
        "No special reader is registered for {.val {state}} / {.val {county}}.",
        "i" = "Register one in {.var rcvr_SPECIAL_READERS} in {.file R/read_special.R}."
      ),
      class = "rcvr_no_special_reader"
    )
  }

  pairs <- rcvr_SPECIAL_READERS[[key]](fs::path_real(path))
  assert_pairs(pairs)
}
