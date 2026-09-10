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

#' Registered readers for counties whose CVR format fits no general parser
#'
#' Names are `"STATE|COUNTY"`, upper case. Each value is a function taking a
#' single `path` and returning a pairs frame. Adding a county means adding one
#' entry here and one test; nothing else in the package changes.
rcvr_SPECIAL_READERS <- list(
  "NEW JERSEY|CUMBERLAND" = read_special_nj_cumberland,
  "TEXAS|DENTON" = read_special_tx_denton,
  "FLORIDA|WALTON" = read_special_fl_walton
)

#' Is a special reader registered for this county?
#'
#' @param state,county County identity, case-insensitive.
#'
#' @return `TRUE` or `FALSE`.
has_special_reader <- function(state, county) {
  special_key(state, county) %in% names(rcvr_SPECIAL_READERS)
}

#' Read a county whose CVR format fits no general parser
#'
#' @param path Path to the county's CVR file or directory.
#' @param state,county County identity, case-insensitive.
#'
#' @return A pairs frame.
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
