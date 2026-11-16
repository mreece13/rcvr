# text of the first node matching `xpath` under `node`, NA when absent
xml_chr <- function(node, xpath) {
  hit <- xml2::xml_find_first(node, xpath)
  if (inherits(hit, "xml_missing")) NA_character_ else xml2::xml_text(hit)
}

# resolve one option node to the candidate it marks, or NULL if unmarked.
# A marked option with write-in markup is "WRITEIN" whether or not the
# write-in text was transcribed (Hart exports an empty <Text/> for an
# unresolved write-in); a marked option with neither a Name nor write-in
# markup is NA, which read_xml_cvr() aborts on.
xml_option <- function(opt) {
  if (!identical(xml_chr(opt, "./Value"), "1")) return(NULL)
  if (!inherits(xml2::xml_find_first(opt, "./WriteInData"), "xml_missing")) {
    return("WRITEIN")
  }
  xml_chr(opt, "./Name")
}

# parse one ballot XML file into long rows
#
# Two layouts occur. The older one has one sibling <Contests> element per
# contest directly under the root, each holding its <Name> and one sibling
# <Options> element per option:
#
#   <Cvr><Contests><Name/><Options><Name/><Value/></Options>...</Contests>...
#
# Hart's CVRDesign.xsd export (Tarrant TX, Nevada County CA, San Joaquin CA,
# Hawaii) wraps them one level deeper:
#
#   <Cvr><Contests><Contest><Name/><Options><Option><Name/><Value/></Option>
#     </Options><Undervotes/><Overvoted/></Contest>...</Contests>...
#
# Both are read with the same XPath union. The default namespace is
# stripped first so the paths need no prefix.
xml_ballot <- function(path, i) {
  x <- xml2::read_xml(path)
  xml2::xml_ns_strip(x)

  # PrecinctSplit is optional; a ballot without one gets an NA precinct,
  # matching the pairs contract, rather than crashing the whole directory
  precinct <- xml_chr(x, "/*/PrecinctSplit/Name")

  contests <- xml2::xml_find_all(x, "/*/Contests/Contest | /*/Contests[Name]")

  rows <- lapply(contests, function(con) {
    contest <- xml_chr(con, "./Name")
    contest_id <- xml_chr(con, "./Id")

    options <- xml2::xml_find_all(con, "./Options/Option | ./Options[not(Option)][Value]")
    resolved <- lapply(options, xml_option)
    # NULL is an unmarked option (it cast no vote, drop it); NA is a *marked*
    # option we cannot identify, kept so read_xml_cvr() can abort on it and
    # it is never confused with "no marks", which collapses to "undervote"
    resolved <- resolved[!vapply(resolved, is.null, logical(1))]
    marked <- as.character(unlist(resolved))

    if (!inherits(xml2::xml_find_first(con, "./Overvoted"), "xml_missing")) {
      # an overvoted contest counts for no candidate, as in read_json_cvr()
      marked <- "overvote"
    } else if (length(marked) == 0) {
      # no marked option (Undervotes, or every option unmarked)
      marked <- "undervote"
    }

    list(contest = rep(contest, length(marked)), raw_candidate = marked,
         contest_id = rep(contest_id, length(marked)))
  })

  contest <- unlist(lapply(rows, `[[`, "contest"), use.names = FALSE)
  raw_candidate <- unlist(lapply(rows, `[[`, "raw_candidate"), use.names = FALSE)
  contest_id <- unlist(lapply(rows, `[[`, "contest_id"), use.names = FALSE)
  if (is.null(contest)) {
    contest <- character(0)
    raw_candidate <- character(0)
    contest_id <- character(0)
  }

  tibble::tibble(
    cvr_id = rep(as.integer(i), length(contest)),
    precinct = rep(precinct, length(contest)),
    contest = stringr::str_replace_all(contest, stringr::fixed("\n"), " "),
    raw_candidate = stringr::str_replace_all(raw_candidate, stringr::fixed("\n"), " "),
    raw_party = NA_character_,
    rank = NA_integer_,
    contest_id = contest_id
  )
}

#' Read a directory of per-ballot XML CVRs
#'
#' One file per ballot sheet; the ballot's identity is its position in the
#' sorted file listing. Hart exports write one file per *sheet* and carry no
#' key linking the sheets of one voter's ballot, so for multi-sheet ballots
#' `cvr_id` identifies a sheet.
#'
#' @param path Path to the directory of `.xml` files.
#'
#' Progress is signalled through \pkg{progressr} about every 1% of files,
#' and nothing is shown unless a handler is enabled. Interactively, wrap the
#' call in `progressr::with_progress()`. progressr is off in non-interactive
#' sessions, so in an `Rscript` (output to a log) run
#' `options(progressr.enable = TRUE)`, `progressr::handlers(global = TRUE)` and
#' `progressr::handlers("cli")` first. The cli handler writes a line with
#' percent, sheet count and ETA every few seconds at most.
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

  # Large Hart exports run for hours (Tarrant TX 2024: 838K sheets, 4.4 h).
  # Report progress about every 1% of sheets through progressr, which shows
  # nothing unless the caller enables a handler (see the docs above).
  n <- length(files)
  step <- max(1L, n %/% 100L)
  p <- progressr::progressor(steps = ceiling(n / step))
  raw <- purrr::imap(files, function(f, i) {
    out <- xml_ballot(f, i)
    if (i %% step == 0L || i == n) {
      p(message = sprintf("XML sheet %s of %s",
                          format(i, big.mark = ","), format(n, big.mark = ",")))
    }
    out
  }) |>
    purrr::list_rbind()

  # Hart names contests without their jurisdiction ("Mayor", "Trustee,
  # Place 1"), so different cities' contests share a <Name> and differ only
  # in <Id>. A name carried by more than one Id gets the Id's first 8
  # characters appended, "Mayor [46075cc4]"; unique names are unchanged.
  shared <- raw |>
    dplyr::filter(!is.na(.data$contest_id)) |>
    dplyr::distinct(.data$contest, .data$contest_id) |>
    dplyr::count(.data$contest) |>
    dplyr::filter(.data$n > 1)
  raw <- raw |>
    dplyr::mutate(contest = dplyr::if_else(
      .data$contest %in% shared$contest & !is.na(.data$contest_id),
      paste0(.data$contest, " [", substr(.data$contest_id, 1, 8), "]"),
      .data$contest
    )) |>
    dplyr::select(-"contest_id")

  # a marked option with neither a Name nor write-in markup is not the same
  # thing as an undervote -- a vote was cast, we just can't identify it -- so
  # it must abort rather than disappear
  unnamed <- dplyr::filter(raw, is.na(raw_candidate))
  if (nrow(unnamed) > 0) {
    cli::cli_abort(
      "{nrow(unnamed)} marked option{?s} in {.file {path}} {?has/have} no {.field Name} and no write-in text.",
      class = "rcvr_unnamed_option"
    )
  }

  assert_pairs(raw)
}
