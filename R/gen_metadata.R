gen_metadata <- function(df, type, path, verbose=FALSE){

  if (verbose) cli::cli_alert_info("Generating metadata")

  # pattern generator for party detection
  gen_patterns <- function(strings) {
    patterns <- c()

    for (s in strings) {
      # Escape special regex characters in the string
      s <- gsub("([\\[\\]\\(\\)\\{\\}\\^\\$\\*\\+\\?\\|\\\\])", "\\\\\\1", s)

      r1 <- paste0("^", s, "$")
      r2 <- paste0("^", s, " ")
      r3 <- paste0(" ", s, "$")
      r4 <- paste0("\\\\(", s, "\\\\)")

      patterns <- c(patterns, r1, r2, r3, r4)
    }

    return(paste(patterns, collapse = "|"))
  }

  # used for detecting merged rows and filling in magnitude
  find_consecutive_ranges <- function(indices) {
    # Find where consecutive sequences break (difference > 1)
    breaks <- which(diff(indices) > 1)

    # Start positions of each sequence
    starts <- c(1, breaks + 1)

    # End positions of each sequence
    ends <- c(breaks, length(indices))

    # Create list of tuples (start_value, end_value)
    ranges <- list()
    for (i in 1:length(starts)) {
      ranges[[i]] <- indices[starts[i]:ends[i]]
    }

    return(ranges)
  }

  if (type == "DELIM" | type == "MULTI-DELIM"){

    meta = df |>
      dplyr::filter(!(contest %in% c(rcvr_DROP_COLS, rcvr_RENAME_COLS))) |>
      dplyr::distinct(contest, raw_candidate, raw_party) |>
      dplyr::mutate(
        "ballot_order" = dplyr::cur_group_id(),
        .by = "contest"
      ) |>
      dplyr::mutate(
        candidate = raw_candidate,
        party = raw_party,
        magnitude = stringr::str_extract(contest, stringr::regex("Vote For=(\\d+)", T), group=1) |> as.integer(),
        party = dplyr::case_when(
          is.na(party) ~ NA_character_,
          stringr::str_detect(party, stringr::regex(gen_patterns(c("DEM", "DFL")), TRUE)) ~ "DEMOCRAT",
          stringr::str_detect(party, stringr::regex(gen_patterns("REP"), TRUE)) ~ "REPUBLICAN",
          stringr::str_detect(party, stringr::regex(gen_patterns(c("CPF", "ACN")), TRUE)) ~ "CONSTITUTION",
          stringr::str_detect(party, stringr::regex(gen_patterns(c("GRN", "MTN")), TRUE)) ~ "GREEN",
          stringr::str_detect(party, stringr::regex(gen_patterns(c("LBR", "LBT", "LIB", "LPN", "NMD")), TRUE)) ~ "LIBERTARIAN",
          stringr::str_detect(party, stringr::regex(gen_patterns(c("PSL", "PGP")), TRUE)) ~ "SOCIALIST",
          stringr::str_detect(party, stringr::regex(gen_patterns("NPA"), TRUE)) ~ "NO PARTY AFFILIATION",
          stringr::str_detect(party, stringr::regex(gen_patterns("PRO"), TRUE)) ~ "PROGRESSIVE",
          stringr::str_detect(party, stringr::regex(gen_patterns(c("IND", "IAP")), TRUE)) ~ "INDEPENDENT",
          stringr::str_detect(party, stringr::regex(gen_patterns("GLC"), TRUE)) ~ "GRASSROOTS-LEGALIZE CANNABIS",
          stringr::str_detect(party, stringr::regex(gen_patterns("NME"), TRUE)) ~ "END THE CORRUPTION",
          stringr::str_detect(party, stringr::regex("Write", TRUE)) ~ "WRITEIN",
          .default = party
        ),
        party = dplyr::case_when(
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("DEM", "DFL")), TRUE)) ~ "DEMOCRAT",
          stringr::str_detect(candidate, stringr::regex(gen_patterns("REP"), TRUE)) ~ "REPUBLICAN",
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("CPF", "ACN")), TRUE)) ~ "CONSTITUTION",
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("GRN", "MTN")), TRUE)) ~ "GREEN",
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("LBR", "LBT", "LIB", "LPN", "NMD")), TRUE)) ~ "LIBERTARIAN",
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("PSL", "PGP")), TRUE)) ~ "SOCIALIST",
          stringr::str_detect(candidate, stringr::regex(gen_patterns("NPA"), TRUE)) ~ "NO PARTY AFFILIATION",
          stringr::str_detect(candidate, stringr::regex(gen_patterns("PRO"), TRUE)) ~ "PROGRESSIVE",
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("IND", "IAP")), TRUE)) ~ "INDEPENDENT",
          stringr::str_detect(candidate, stringr::regex(gen_patterns("GLC"), TRUE)) ~ "GRASSROOTS-LEGALIZE CANNABIS",
          stringr::str_detect(candidate, stringr::regex(gen_patterns("NME"), TRUE)) ~ "END THE CORRUPTION",
          stringr::str_detect(candidate, stringr::regex("Write", TRUE)) ~ "WRITEIN",
          stringr::str_detect(candidate, stringr::regex("undervote|overvote|No image found", ignore_case=TRUE)) ~ NA_character_,
          is.na(candidate) ~ NA_character_,
          .default = party
        ),
        # clean candidate up a bit at the beginning, so we can catch more party information
        candidate = stringr::str_remove_all(candidate, "^$|^NA$|^N/A$|\\([^)]*\\)$|[\\p{Mn}]") |> stringi::stri_trans_nfd(),
        # detect party again after some cleanup
        party = dplyr::case_when(
          !is.na(party) ~ party,
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("DEM", "DFL")), TRUE)) ~ "DEMOCRAT",
          stringr::str_detect(candidate, stringr::regex(gen_patterns("REP"), TRUE)) ~ "REPUBLICAN",
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("CPF", "ACN")), TRUE)) ~ "CONSTITUTION",
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("GRN", "MTN")), TRUE)) ~ "GREEN",
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("LBR", "LBT", "LIB", "LPN", "NMD")), TRUE)) ~ "LIBERTARIAN",
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("PSL", "PGP")), TRUE)) ~ "SOCIALIST",
          stringr::str_detect(candidate, stringr::regex(gen_patterns("NPA"), TRUE)) ~ "NO PARTY AFFILIATION",
          stringr::str_detect(candidate, stringr::regex(gen_patterns("PRO"), TRUE)) ~ "PROGRESSIVE",
          stringr::str_detect(candidate, stringr::regex(gen_patterns(c("IND", "IAP")), TRUE)) ~ "INDEPENDENT",
          stringr::str_detect(candidate, stringr::regex(gen_patterns("GLC"), TRUE)) ~ "GRASSROOTS-LEGALIZE CANNABIS",
          stringr::str_detect(candidate, stringr::regex(gen_patterns("NME"), TRUE)) ~ "END THE CORRUPTION",
          stringr::str_detect(candidate, stringr::regex("Write", TRUE)) ~ "WRITEIN",
          stringr::str_detect(candidate, stringr::regex("undervote|overvote|No image found", ignore_case=TRUE)) ~ NA_character_,
          is.na(candidate) ~ NA_character_,
          .default = party
        ),
        # this regex does several things:
        # - remove any empty strings
        # - remove any weird NA characters
        # - remove content that is between parens
        # - remove any non-marking unicode characters, such as diacritics
        # - remove punctuation
        # - squishes
        candidate = stringr::str_remove_all(candidate, stringr::regex(gen_patterns(c("DEM", "DFL", "REP", "CPF", "ACN", "GRN", "MTN", "LBR", "LBT", "LIB", "LPN", "NMD", "PSL", "PGP", "NPA", "PRO", "IND", "IAP", "GLC", "NME", "No image found")), TRUE)),
        candidate = stringr::str_remove_all(candidate, "^$|^NA$|^N/A$|\\([^)]*\\)$|[\\p{Mn}]"),
        candidate = stringr::str_squish(candidate),
        candidate = ifelse(stringr::str_detect(candidate, stringr::regex("Write", T)), "WI", candidate),
        party = dplyr::case_when(
          stringr::str_detect(candidate, stringr::regex("undervote|overvote|No image found", T)) ~ NA_character_,
          .default = party
        )
      ) |>
      dplyr::filter(
        !stringr::str_detect(candidate, stringr::regex("^undervote$|^overvote$|^WI$", T))
      ) |>
      dplyr::arrange(ballot_order)

  }
  else if (type == "JSON") {

    lookup_district <- jsonlite::read_json(paste0(path, "/DistrictManifest.json")) |>
      data.table::as.data.table() |>
      tidyr::hoist(List,
        district = "Description",
        id = "Id"
      ) |>
      dplyr::select(-Version, -List)

    lookup_contests <- jsonlite::read_json(paste0(path, "/ContestManifest.json")) |>
      data.table::as.data.table() |>
      tidyr::hoist(List,
        contest = "Description",
        district_of_contest = "DistrictId",
        id = "Id"
      ) |>
      dplyr::select(-Version, -List) |>
      dplyr::left_join(lookup_district, dplyr::join_by(district_of_contest == id))

    lookup_candidates <- jsonlite::read_json(paste0(path, "/CandidateManifest.json")) |>
      data.table::as.data.table() |>
      tidyr::hoist(List,
        candidate = "Description",
        candidate_type = "Type",
        id = "Id"
      ) |>
      dplyr::select(-Version, -List)

    lookup_party <- jsonlite::read_json(paste0(path, "/PartyManifest.json")) |>
      data.table::as.data.table() |>
      tidyr::hoist(List,
        party = "Description",
        id = "Id"
      ) |>
      dplyr::select(-Version, -List) |>
      dplyr::mutate(party = dplyr::case_when(
        stringr::str_detect(party, stringr::regex("REP", ignore_case = TRUE)) ~ "REPUBLICAN",
        stringr::str_detect(party, stringr::regex("DEM|Democrat|Democratic", ignore_case = TRUE)) ~ "DEMOCRAT",
        stringr::str_detect(party, stringr::regex("LBT|Libertarian|Lib|LPN", ignore_case = TRUE)) ~ "LIBERTARIAN",
        stringr::str_detect(party, stringr::regex("IND|IAP|IPN", ignore_case = TRUE)) ~ "INDEPENDENT",
        stringr::str_detect(party, stringr::regex("NON|NPN|Nonpartisan", ignore_case = TRUE)) ~ "NONPARTISAN",
        .default = stringr::str_to_upper(party)
      ))

    lookup_precinct_portion <- jsonlite::read_json(paste0(path, "/PrecinctPortionManifest.json")) |>
      data.table::as.data.table() |>
      tidyr::hoist(List,
        precinct_portion = "Description",
        precinct = "PrecinctId",
        id = "Id"
      ) |>
      dplyr::select(-Version, -List)

    meta = df |>
      dplyr::filter(isCurrent) |>
      dplyr::mutate(
        cvr_id = dplyr::cur_group_id(),
        magnitude = as.integer(rank),
        .by = c(batchId, cardId, tabulatorId, recordId)
      ) |>
      dplyr::left_join(lookup_candidates, c("candidateId" = "id")) |>
      dplyr::left_join(lookup_contests, c("contestId" = "id")) |>
      dplyr::left_join(lookup_party, c("partyId" = "id")) |>
      dplyr::left_join(lookup_precinct_portion, c("precinctPortionId" = "id")) |>
      dplyr::select(
        contest, district, candidate, party_detailed=party, magnitude
      ) |>
      dplyr::distinct()

    # df = df[
    #   isCurrent == TRUE
    # ][
    #   , `:=` (
    #     cvr_id = .GRP,
    #     magnitude = as.character(rank)
    #   ),
    #   by = list(batchId, cardId, tabulatorId, recordId)
    # ][
    #   lookup_candidates, on = c("candidateId" = "id")
    # ][
    #   lookup_contests, on = c("contestId" = "id")
    # ][
    #   lookup_party, on = c("partyId" = "id")
    # ][
    #   lookup_precinct_portion, on = c("precinctPortionId" = "id")
    # ][
    #   , list(
    #     contest, district, candidate, party_detailed=party, magnitude
    #   )
    # ]
    #
    # meta = dplyr::distinct(df, contest, district, candidate, party_detailed, magnitude)

  }

  if (isTRUE(any(meta$magnitude > 0, na.rm=TRUE))){

    cs = dplyr::pull(meta, contest)
    cs_ind = which(stringr::str_detect(cs, "^\\.\\.\\.\\d+$"), cs)
    ranges = find_consecutive_ranges(cs_ind)

    mags = rep.int(0, length(cs))

    for (r in ranges){
      mags[c(r[1]-1, r)] = cs[c(r[1]-1, r)] |> unique() |> length()
    }

    meta = meta |>
      dplyr::mutate(
        magnitude2 = mags,
        magnitude2 = dplyr::na_if(magnitude2, 0)
      ) |>
      dplyr::group_by(contest) |>
      tidyr::fill(magnitude2, .direction = "up") |>
      dplyr::ungroup() |>
      dplyr::mutate(
        magnitude2 = tidyr::replace_na(magnitude2, 1),
        magnitude = dplyr::coalesce(magnitude, magnitude2),
        magnitude2 = NULL
      )
  }

  dplyr::bind_rows(
    meta,
    tibble::tibble(
      office = character(), district = character()
    )
  )

}
