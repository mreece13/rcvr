# read one Dominion manifest and return its `List` element
read_manifest_list <- function(path, name) {
  f <- fs::path(path, name)
  if (!fs::file_exists(f)) {
    cli::cli_abort(
      "Missing Dominion manifest {.file {f}}",
      class = "rcvr_missing_manifest"
    )
  }
  jsonlite::read_json(f)$List
}

# pull one field out of every manifest entry, tolerating its absence
manifest_field <- function(lst, field, ptype = character(1)) {
  vapply(
    lst,
    function(x) {
      v <- x[[field]]
      if (is.null(v)) return(if (is.character(ptype)) NA_character_ else NA_integer_)
      if (is.character(ptype)) as.character(v) else as.integer(v)
    },
    ptype
  )
}

#' Read the Dominion manifests that resolve ids to strings
#'
#' @param path Path to a directory of Dominion JSON exports.
#'
#' @return A named list of tibbles: `candidates`, `contests`, `parties`,
#'   `precincts`.
read_dominion_manifests <- function(path) {
  districts_raw <- read_manifest_list(path, "DistrictManifest.json")
  districts <- tibble::tibble(
    id = manifest_field(districts_raw, "Id", integer(1)),
    district = manifest_field(districts_raw, "Description")
  )

  contests_raw <- read_manifest_list(path, "ContestManifest.json")
  contests <- tibble::tibble(
    id = manifest_field(contests_raw, "Id", integer(1)),
    contest = manifest_field(contests_raw, "Description"),
    district_id = manifest_field(contests_raw, "DistrictId", integer(1)),
    # `VoteFor` is the number of seats to be filled: this is magnitude, and it
    # is the only place any CVR format states it explicitly
    magnitude = manifest_field(contests_raw, "VoteFor", integer(1))
  ) |>
    dplyr::left_join(districts, dplyr::join_by(district_id == id)) |>
    dplyr::select(id, contest, district, magnitude)

  candidates_raw <- read_manifest_list(path, "CandidateManifest.json")
  candidates <- tibble::tibble(
    id = manifest_field(candidates_raw, "Id", integer(1)),
    raw_candidate = manifest_field(candidates_raw, "Description")
  )

  parties_raw <- read_manifest_list(path, "PartyManifest.json")
  parties <- tibble::tibble(
    id = manifest_field(parties_raw, "Id", integer(1)),
    raw_party = manifest_field(parties_raw, "Description")
  )

  precincts_raw <- read_manifest_list(path, "PrecinctPortionManifest.json")
  precincts <- tibble::tibble(
    id = manifest_field(precincts_raw, "Id", integer(1)),
    precinct = manifest_field(precincts_raw, "Description")
  )

  list(
    candidates = candidates,
    contests = contests,
    parties = parties,
    precincts = precincts
  )
}

#' Manifest-derived seed values for a JSON county
#'
#' `district` and `magnitude` come from the Dominion manifests rather than
#' from regex. They are seeds like any other: the store owns them once written.
#'
#' @param path Path to a directory of Dominion JSON exports.
#'
#' @return A tibble with `contest`, `district`, `magnitude`.
json_seed_context <- function(path) {
  read_dominion_manifests(path)$contests |>
    dplyr::select(contest, district, magnitude) |>
    dplyr::distinct()
}

#' Read a directory of Dominion JSON CVRs
#'
#' @param path Path to the directory holding `CvrExport*.json` and the
#'   Dominion manifests.
#'
#' @return A pairs frame.
read_json_cvr <- function(path) {
  rlang::check_installed("dominionCVR", reason = "`dominionCVR` is needed to parse JSON files")

  path <- fs::path_real(path)
  files <- list.files(
    path = path,
    pattern = "Cvr.*\\.json$|CVR.*\\.json$",
    full.names = TRUE
  )
  if (length(files) == 0) {
    cli::cli_abort(
      "No CVR export files found in {.file {path}}",
      class = "rcvr_no_files"
    )
  }

  m <- read_dominion_manifests(path)

  joined <- dominionCVR::extract_cvr(files) |>
    tibble::as_tibble() |>
    dplyr::filter(isCurrent) |>
    dplyr::mutate(
      cvr_id = dplyr::cur_group_id(),
      .by = c(batchId, cardId, tabulatorId, recordId)
    ) |>
    dplyr::left_join(m$candidates, dplyr::join_by(candidateId == id)) |>
    dplyr::left_join(dplyr::select(m$contests, id, contest), dplyr::join_by(contestId == id)) |>
    dplyr::left_join(m$parties, dplyr::join_by(partyId == id)) |>
    dplyr::left_join(m$precincts, dplyr::join_by(precinctPortionId == id))

  # A left_join() against a manifest silently turns an unresolved id into
  # NA; assert_pairs() tolerates a partially-NA column, and the downstream
  # metadata join then drops those rows without a trace. -1L is the
  # legitimate "no mark"/"no party" sentinel (handled below), so only an id
  # that is neither -1L nor resolved is an error: a version mismatch, a
  # truncated manifest, or a corrupt export. Check each id/manifest pair
  # separately so the abort names the manifest at fault.
  unresolved <- list(
    CandidateManifest = dplyr::filter(joined, candidateId != -1L, is.na(raw_candidate)),
    ContestManifest = dplyr::filter(joined, contestId != -1L, is.na(contest)),
    PartyManifest = dplyr::filter(joined, partyId != -1L, is.na(raw_party)),
    PrecinctPortionManifest = dplyr::filter(joined, precinctPortionId != -1L, is.na(precinct))
  )
  bad_manifest <- names(unresolved)[vapply(unresolved, nrow, integer(1)) > 0]
  if (length(bad_manifest) > 0) {
    id_col <- c(
      CandidateManifest = "candidateId",
      ContestManifest = "contestId",
      PartyManifest = "partyId",
      PrecinctPortionManifest = "precinctPortionId"
    )
    manifest <- bad_manifest[1]
    bad_rows <- unresolved[[manifest]]
    ids <- unique(bad_rows[[id_col[[manifest]]]])
    cli::cli_abort(
      c(
        "{length(ids)} id{?s} in the CVR export could not be resolved against {.file {manifest}.json}.",
        "i" = "Unresolved id{?s}: {.val {utils::head(ids, 10)}}"
      ),
      class = "rcvr_unresolved_ids",
      manifest = manifest,
      ids = ids,
      unresolved = bad_rows
    )
  }

  pairs <- joined |>
    dplyr::mutate(
      # extract_marks() emits -1 for a contest with no marks, which is an
      # undervote, and -1 rank/party alongside it
      raw_candidate = dplyr::if_else(candidateId == -1L, "undervote", raw_candidate),
      raw_party = dplyr::if_else(partyId == -1L, NA_character_, raw_party),
      rank = dplyr::if_else(rank == -1L, NA_integer_, as.integer(rank)),
      cvr_id = as.integer(cvr_id),
      precinct = as.character(precinct)
    ) |>
    dplyr::select(cvr_id, precinct, contest, raw_candidate, raw_party, rank) |>
    dplyr::distinct()

  assert_pairs(pairs)
}
