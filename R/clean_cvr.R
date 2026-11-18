#' Attach store values to a pairs frame
#'
#' A left join, guarded by an anti-join check. A raw pair with no store row is
#' a hard error for that county: it means the store has not been seeded or
#' reconciled for this file, and silently dropping the rows is precisely the
#' failure mode this package exists to remove.
#'
#' Pairs matching `rcvr_UNSEEDED_RE` (undervote, overvote, write-in, `"0"`)
#' are exempt — they are synthesised by the reader and `gen_metadata()`
#' deliberately never seeds them into the store.
#'
#' @param pairs A pairs frame.
#' @param metadata A store slice, already filtered to one
#'   `(election, state, county)`.
#' @param state,county Used only in the error message.
#'
#' @return The pairs frame with store columns attached.
join_metadata <- function(pairs, metadata, state = NA, county = NA) {
  if (is.null(metadata)) {
    cli::cli_abort(
      "{.var metadata} is required unless {.var generate_metadata} or {.var metadata_only} is {.code TRUE}.",
      class = "rcvr_no_metadata"
    )
  }

  # the slice arrives carrying the store key columns; they are constant within
  # a slice and would collide with the caller's own columns downstream.
  # raw_party is dropped too: the pairs frame already carries its own
  # raw_party (the reader's copy, authoritative), and the store's copy is
  # identical by construction, so keeping both would produce
  # raw_party.x/raw_party.y from left_join() below
  meta <- dplyr::select(
    metadata,
    -tidyselect::any_of(c("election", "state", "county", "raw_party"))
  )

  # gen_metadata()'s dplyr::distinct(contest, raw_candidate, raw_party) can
  # emit two store rows sharing (contest, raw_candidate) when a county spells
  # one candidate's party two ways. The left join below then fans a single
  # ballot row out into two, silently inflating vote totals -- so this must
  # be caught before the join runs, not after.
  dupes <- dplyr::filter(
    meta,
    dplyr::n() > 1,
    .by = c(contest, raw_candidate)
  )
  if (nrow(dupes) > 0) {
    shown <- utils::head(dplyr::distinct(dupes, contest, raw_candidate), 10)
    cli::cli_abort(
      c(
        "{nrow(shown)} {.val {'(contest, raw_candidate)'}} pair{?s} in the metadata store {?has/have} more than one row.",
        "x" = paste0(shown$contest, " / ", shown$raw_candidate),
        "i" = "A left join against a duplicated pair fans out ballot rows; reconcile the store for this county before cleaning it."
      ),
      class = "rcvr_duplicate_metadata",
      pairs = dupes
    )
  }

  # an NA raw_candidate is never exempt: str_detect(NA, ...) is NA, and
  # dplyr::filter() drops NA conditions, so without coalescing to FALSE an
  # NA-keyed pair would silently skip the anti-join and survive the left
  # join with every store column NA — exactly the silent loss this
  # function exists to prevent
  joinable <- dplyr::filter(
    pairs,
    !dplyr::coalesce(stringr::str_detect(raw_candidate, rcvr_UNSEEDED_RE), FALSE)
  )

  unmatched <- dplyr::anti_join(
    dplyr::distinct(joinable, contest, raw_candidate),
    meta,
    dplyr::join_by("contest", "raw_candidate")
  )

  if (nrow(unmatched) > 0) {
    shown <- utils::head(unmatched, 10)
    cli::cli_abort(
      c(
        "{nrow(unmatched)} raw pair{?s} in {.val {state}} / {.val {county}} {?has/have} no row in the metadata store.",
        "x" = paste0(shown$contest, " / ", shown$raw_candidate),
        "i" = "Seed and reconcile the store for this county before cleaning it."
      ),
      class = "rcvr_unmatched_pairs",
      pairs = unmatched
    )
  }

  dplyr::left_join(pairs, meta, dplyr::join_by("contest", "raw_candidate")) |>
    # writein/undervote/overvote rows inherit their contest's office metadata
    dplyr::group_by(contest) |>
    tidyr::fill(office, district, magnitude, .direction = "downup") |>
    dplyr::ungroup()
}

#' Read a CVR into the pairs frame
#'
#' Resolves the reader, extracts a zipped export (CVR members only) to a
#' temporary directory for the duration of the call, and runs the reader.
#' `clean_cvr()` calls this; call it directly to read once and pass the
#' result to several `clean_cvr(pairs = )` calls (a seed run and a clean run)
#' instead of re-reading and re-extracting the export each time.
#'
#' @inheritParams clean_cvr
#'
#' @return A pairs frame, carrying the resolved reader type and path as
#'   attributes (`rcvr_type`, `rcvr_path`) and, for JSON exports, the
#'   manifest context `gen_metadata()` needs once the extraction is gone.
#' @export
read_cvr <- function(path, type = NULL, state = NA_character_, county = NA_character_,
                     verbose = FALSE) {
  if (!checkmate::test_string(path, na.ok = FALSE)) {
    cli::cli_abort(
      "{.arg path} must be a single non-NA string.",
      class = "rcvr_bad_path"
    )
  }

  # a zipped export is extracted (CVR members only) to a temp dir for the
  # duration of this call, so expanded exports never have to live on disk
  # next to the raw data. `outer.zip::inner.zip` names a nested export.
  zip_re <- stringr::regex("\\.zip(::.+)?$", ignore_case = TRUE)
  outer <- strsplit(path, "::", fixed = TRUE)[[1]][1]
  if (stringr::str_detect(path, zip_re) && fs::is_file(outer)) {
    exdir <- tempfile("rcvr_zip_")
    on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
    if (verbose) cli::cli_alert_info("Extracting {.file {path}}")
    path <- unzip_cvr(path, exdir = exdir)
  }

  path <- fs::path_real(path)

  resolved <- resolve_reader(path, type = type, verbose = FALSE)
  path <- resolved$path
  type <- resolved$type

  valid_types <- c("DELIM", "DELIM-MULTI", "JSON", "XML", "SPECIAL")
  if (is.null(type) || !checkmate::test_string(type) || !(type %in% valid_types)) {
    cli::cli_abort(
      "Could not resolve a CVR type for {.file {path}}.",
      class = "rcvr_bad_type"
    )
  }

  if (type == "SPECIAL" && (is.na(state) || is.na(county))) {
    cli::cli_abort(
      "{.var state} and {.var county} are required for {.val special} CVRs.",
      class = "rcvr_missing_county"
    )
  }

  if (verbose) cli::cli_alert_info("Cleaning {.file {path}}")

  if (type == "DELIM") {
    # a wide CSV costs ~8 bytes per cell read whole; stream large ones
    big <- stringr::str_detect(path, stringr::regex("\\.csv$", ignore_case = TRUE)) &&
      isTRUE(file.size(path) > getOption("rcvr.chunk_bytes", 1e9))
    pairs <- if (big) {
      read_delim_chunked(path)
    } else {
      read_delim_cvr(path) |> pairs_from_delim(path)
    }
  } else if (type == "DELIM-MULTI") {
    pairs <- read_delim_multi_cvr(path) |> pairs_from_delim(path)
  } else if (type == "JSON") {
    pairs <- read_json_cvr(path)
    # the manifests live in the (possibly temporary) export directory
    attr(pairs, "rcvr_json_context") <- json_seed_context(path)
  } else if (type == "XML") {
    pairs <- read_xml_cvr(path)
  } else if (type == "SPECIAL") {
    pairs <- read_special_cvr(path, state, county)
  }

  # the delimited and special readers all go through data.table::fread(),
  # which leaves an escaped quote doubled (see undouble_quotes())
  if (type %in% c("DELIM", "DELIM-MULTI", "SPECIAL")) {
    pairs <- undouble_quotes(pairs)
  }
  pairs <- trim_names(pairs)

  attr(pairs, "rcvr_type") <- type
  attr(pairs, "rcvr_path") <- as.character(path)
  pairs
}

# dplyr verbs carry a pairs frame's reader attributes (type, path, manifest
# context) onto their results; they describe the read, not the output
strip_rcvr_attrs <- function(x) {
  for (a in c("rcvr_type", "rcvr_path", "rcvr_json_context")) attr(x, a) <- NULL
  x
}

#' Main Cleaning Function
#'
#' @param path Path to the CVR file, directory, or `.zip`. A nested zip is
#'   named as `"outer.zip::inner.zip"`. Ignored when `pairs` is given.
#' @param type (optional) The declared CVR type from the registry: one of `"delim"`, `"json"`, `"xml"`, `"special"`. When `NULL` (default) the type is sniffed from the files on disk, which aborts on directories holding more than one file type.
#' @param metadata (optional) Existing metadata to merge into the CVR
#' @param generate_metadata Whether to generate new metadata using a series of rules and then append these to the CVR. Default is `FALSE`.
#' @param return_metadata Return the metadata generated by the function for the user to examine? Default `FALSE`.
#' @param metadata_only Only return the metadata generated, and skip returning the file itself? Useful for getting a blank slate of metadata that users could then manually adjust before returning via the `metadata=` argument. Default `FALSE`.
#' @param verbose Verbose output? Default `FALSE`
#' @param write_path (optional) Path to write a parquet file, in which case the function only returns a path
#' @param election The election label, e.g. `"2020 General"`. Required when generating metadata.
#' @param state The state name, upper case. Required when generating metadata.
#' @param county The county name, upper case. Required when generating metadata.
#' @param pairs (optional) A pairs frame from [read_cvr()], to skip reading
#'   the CVR again.
#'
#' @returns Either a `tibble` with the cleaned CVR (`return_metadata=FALSE`) or a list containing both the cleaned CVR and the generated metadata (`generate_metadata=TRUE` and `return_metadata=FALSE`) or just the generated metadata (`metadata_only=TRUE`) or a path to the output file (`write_path` is non-NULL).
#' @export
#'
clean_cvr <- function(
  path=NA,
  type=NULL,
  metadata=NULL,
  generate_metadata=FALSE,
  return_metadata=FALSE,
  metadata_only=FALSE,
  verbose=TRUE,
  write_path=NULL,
  election=NA_character_,
  state=NA_character_,
  county=NA_character_,
  pairs=NULL
) {

  if (!is.null(metadata) && isTRUE(generate_metadata)) {
    cli::cli_inform(
      "{.var metadata} is non-NULL so {.var generate_metadata} is being ignored."
    )
    generate_metadata <- FALSE
  }

  if (is.null(pairs)) {
    # fread only warns when a row has the wrong field count ("Stopped early
    # on line N") and returns the rows before it, so a malformed file would
    # pass as a short county (Passaic NJ 2024: 3,531 of 204,153 ballots)
    pairs <- withCallingHandlers(
      read_cvr(path, type = type, state = state, county = county, verbose = verbose),
      warning = function(w) {
        if (grepl("Stopped early on line", conditionMessage(w), fixed = TRUE)) {
          cli::cli_abort(c("The CSV reader stopped early; later rows were not read.",
                           "x" = conditionMessage(w)), class = "rcvr_truncated_read")
        }
      }
    )
  } else {
    assert_pairs(pairs)
  }
  type <- attr(pairs, "rcvr_type")
  path <- attr(pairs, "rcvr_path")
  if (is.null(type)) {
    cli::cli_abort(
      "{.arg pairs} must come from {.fn read_cvr} (it carries the reader type).",
      class = "rcvr_bad_type"
    )
  }

  if (isTRUE(generate_metadata) || isTRUE(metadata_only)) {
    metadata <- gen_metadata(pairs, type, path, election, state, county, verbose) |>
      strip_rcvr_attrs()
  }
  if (isTRUE(metadata_only)) return(metadata)

  clean <- join_metadata(pairs, metadata, state = state, county = county) |>
    strip_rcvr_attrs()

  if (isTRUE(return_metadata)) {
    return(list(clean = clean, metadata = metadata))
  }

  if (!is.null(write_path)) {
    rlang::check_installed("arrow", reason = "`arrow` is needed to write Parquet files")

    fs::dir_create(fs::path_dir(write_path))
    arrow::write_parquet(clean, write_path)
    return(write_path)
  }

  return(clean)

}
