# parse one ballot XML file into long rows
#
# The vendor format has multiple sibling <Contests> elements directly under
# the root, so xml2::as_list() yields a list with duplicate "Contests"
# names. tidyr::unnest_wider() requires unique names and errors on that
# shape (verified against the fixtures), so the ported cvrs implementation
# (which used unnest_wider()/hoist() throughout) does not run as written.
# This walks the parsed list directly instead, which tolerates the
# duplicate-name siblings tidyr's wide-unnest cannot.
xml_ballot <- function(path, i) {
  x <- xml2::read_xml(path)
  root <- xml2::as_list(x)[[1]]

  precinct <- purrr::pluck(root, "PrecinctSplit", "Name", 1)

  contests <- root[names(root) == "Contests"]

  rows <- purrr::map(contests, function(con) {
    contest <- purrr::pluck(con, "Name", 1)
    undervote_flag <- purrr::pluck(con, "Undervotes", 1)
    writein_name <- purrr::pluck(con, "Options", "WriteInData", "Text", 1)
    candidate_name <- purrr::pluck(con, "Options", "Name", 1)

    undervote <- dplyr::if_else(undervote_flag == "1", "undervote", NA_character_)
    writein_name <- dplyr::if_else(!is.null(writein_name), "WRITEIN", NA_character_)

    tibble::tibble(
      contest = contest,
      raw_candidate = dplyr::coalesce(undervote, writein_name, candidate_name)
    )
  }) |>
    purrr::list_rbind()

  rows |>
    dplyr::mutate(
      cvr_id = as.integer(i),
      precinct = as.character(precinct),
      contest = stringr::str_replace_all(contest, stringr::fixed("\n"), " "),
      raw_candidate = stringr::str_replace_all(raw_candidate, stringr::fixed("\n"), " "),
      raw_party = NA_character_,
      rank = NA_integer_
    ) |>
    dplyr::select(cvr_id, precinct, contest, raw_candidate, raw_party, rank)
}

#' Read a directory of per-ballot XML CVRs
#'
#' One file per ballot; the ballot's identity is its position in the sorted
#' file listing.
#'
#' @param path Path to the directory of `.xml` files.
#'
#' @return A pairs frame. `raw_party` and `rank` are always `NA` — this
#'   vendor's format carries neither.
read_xml_cvr <- function(path) {
  path <- fs::path_real(path)

  files <- list.files(
    path = path,
    pattern = "\\.xml$",
    recursive = TRUE,
    full.names = TRUE
  ) |> sort()

  if (length(files) == 0) {
    cli::cli_abort(
      "No XML files found in {.file {path}}",
      class = "rcvr_no_files"
    )
  }

  pairs <- purrr::imap(files, xml_ballot) |>
    purrr::list_rbind() |>
    # an undervote row takes precedence over an empty candidate name
    dplyr::filter(!is.na(raw_candidate))

  assert_pairs(pairs)
}
