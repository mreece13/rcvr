# pull a single scalar out of a pluck() result, treating NULL/empty as NA
pluck_chr <- function(x, ...) {
  v <- purrr::pluck(x, ...)
  if (is.null(v)) NA_character_ else as.character(v)
}

# a contest may list one Options sibling per candidate (vote-for-N contests,
# or a format that lists every option rather than only the marked one), so
# every "Options" name under a contest must be read, not just the first
xml_option <- function(opt) {
  value <- pluck_chr(opt, "Value", 1)
  marked <- !is.na(value) && identical(value, "1")
  if (!marked) return(NULL)

  writein_text <- purrr::pluck(opt, "WriteInData", "Text", 1)
  if (!is.null(writein_text)) return("WRITEIN")
  pluck_chr(opt, "Name", 1)
}

# parse one ballot XML file into long rows
#
# The vendor format has multiple sibling <Contests> elements directly under
# the root (and, within a contest, multiple sibling <Options> elements), so
# xml2::as_list() yields lists with duplicate names. tidyr::unnest_wider()
# requires unique names and errors on that shape (verified against the
# fixtures), so the ported cvrs implementation (which used
# unnest_wider()/hoist() throughout, and which also read only the first
# <Options> sibling regardless of which one was marked) does not run
# correctly as written. This walks the parsed list directly instead, which
# tolerates the duplicate-name siblings tidyr's wide-unnest cannot, and
# reads every <Options> sibling of a contest rather than only the first.
xml_ballot <- function(path, i) {
  x <- xml2::read_xml(path)
  root <- xml2::as_list(x)[[1]]

  # PrecinctSplit is optional; a ballot without one gets an NA precinct,
  # matching the pairs contract, rather than crashing the whole directory
  precinct_split <- purrr::pluck(root, "PrecinctSplit")
  precinct <- if (is.null(precinct_split)) {
    NA_character_
  } else {
    pluck_chr(precinct_split, "Name", 1)
  }

  contests <- root[names(root) == "Contests"]

  rows <- purrr::map(contests, function(con) {
    contest <- pluck_chr(con, "Name", 1)

    options <- con[names(con) == "Options"]
    marked <- purrr::map_chr(options, function(opt) {
      candidate <- xml_option(opt)
      if (is.null(candidate)) NA_character_ else candidate
    }) |>
      unname()
    marked <- marked[!is.na(marked)]

    # no marked option (Undervotes == "1", or every Options sibling
    # unmarked) becomes a single undervote row
    if (length(marked) == 0) marked <- "undervote"

    tibble::tibble(contest = contest, raw_candidate = marked)
  }) |>
    purrr::list_rbind()

  rows |>
    dplyr::mutate(
      cvr_id = as.integer(i),
      precinct = precinct,
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
#' @export
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
