# the exact column set of a seed row, in order. Annotation and provenance
# columns (`type`, `topic`, `subtopic`, `prop_text`, `source`, `notes`) are
# added by the migration, not here.
rcvr_SEED_COLS <- c(
  "election", "state", "county", "contest", "raw_candidate",
  "raw_party", "ballot_order",
  "office", "district", "candidate", "party_detailed", "magnitude",
  "nonpartisan", "drop"
)

# pattern generator for party detection
gen_patterns <- function(strings) {
  patterns <- c()

  for (s in strings) {
    s <- gsub("([\\[\\]\\(\\)\\{\\}\\^\\$\\*\\+\\?\\|\\\\])", "\\\\\\1", s)

    r1 <- paste0("^", s, "$")
    r2 <- paste0("^", s, " ")
    r3 <- paste0(" ", s, "$")
    # NB: fixed from the pre-existing "\\\\(" / "\\\\)" (which produced a
    # literal backslash + an unescaped, unterminated capture group instead of
    # an escaped literal paren) — that bug meant "(LBT)"-style parenthetical
    # abbreviations never matched. Pre-existing defect, fixed minimally here
    # because the Task 4 test suite exercises this path directly.
    r4 <- paste0("\\(", s, "\\)")

    patterns <- c(patterns, r1, r2, r3, r4)
  }

  paste(patterns, collapse = "|")
}

# abbreviation -> canonical party name. The single definition; both the party
# field and the candidate string are matched against it.
rcvr_PARTY_PATTERNS <- list(
  "DEMOCRAT" = c("DEM", "DFL"),
  "REPUBLICAN" = "REP",
  "CONSTITUTION" = c("CPF", "ACN"),
  "GREEN" = c("GRN", "MTN"),
  "LIBERTARIAN" = c("LBR", "LBT", "LIB", "LPN", "NMD"),
  "SOCIALIST" = c("PSL", "PGP"),
  "NO PARTY AFFILIATION" = "NPA",
  "NONPARTISAN" = c("NON", "NPN", "NONPARTISAN"),
  "PROGRESSIVE" = "PRO",
  "INDEPENDENT" = c("IND", "IAP"),
  "GRASSROOTS-LEGALIZE CANNABIS" = "GLC",
  "END THE CORRUPTION" = "NME"
)

# Dominion manifests carry full party names ("Democratic Party") rather than
# the abbreviations in rcvr_PARTY_PATTERNS. This is an *exact* lookup (not a
# prefix or substring match) keyed on the normalised full name (upper-cased,
# squished, trailing " PARTY" stripped), so "Democratic-Republican Party"
# never collides with "DEMOCRAT". Covers the parties Dominion manifests
# actually use, plus the nonpartisan spellings already in rcvr_PARTY_PATTERNS.
rcvr_PARTY_FULL_NAMES <- c(
  "DEMOCRATIC" = "DEMOCRAT",
  "DEMOCRAT" = "DEMOCRAT",
  "REPUBLICAN" = "REPUBLICAN",
  "LIBERTARIAN" = "LIBERTARIAN",
  "GREEN" = "GREEN",
  "CONSTITUTION" = "CONSTITUTION",
  "INDEPENDENT" = "INDEPENDENT",
  "NONPARTISAN" = "NONPARTISAN",
  "NON-PARTISAN" = "NONPARTISAN",
  "NO PARTY AFFILIATION" = "NO PARTY AFFILIATION",
  "UNAFFILIATED" = "NO PARTY AFFILIATION"
)

# normalise a full manifest party string for exact lookup in
# rcvr_PARTY_FULL_NAMES: upper-case, squish whitespace, drop a trailing
# " PARTY"
normalize_party_full_name <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_squish() |>
    stringr::str_remove(" PARTY$")
}

#' Seed a `party_detailed` value from the raw party and candidate strings
#'
#' The party abbreviation may live in the party field, embedded in the
#' candidate name, or nowhere. This is a **seed**: the store owns the value
#' once it has been written once.
#'
#' @param party Raw party string (may be `NA`).
#' @param candidate Raw candidate string (may be `NA`).
#'
#' @return A character vector of canonical party names, or `NA`.
seed_party <- function(party, candidate) {
  detect_in <- function(x, out) {
    for (canonical in names(rcvr_PARTY_PATTERNS)) {
      hit <- !is.na(x) & is.na(out) &
        stringr::str_detect(x, stringr::regex(gen_patterns(rcvr_PARTY_PATTERNS[[canonical]]), TRUE))
      out[hit] <- canonical
    }
    hit <- !is.na(x) & is.na(out) & stringr::str_detect(x, stringr::regex("Write", TRUE))
    out[hit] <- "WRITEIN"
    out
  }

  out <- rep(NA_character_, max(length(party), length(candidate)))
  party <- rep_len(party, length(out))
  candidate <- rep_len(candidate, length(out))

  out <- detect_in(party, out)
  out <- detect_in(candidate, out)

  # Pre-existing gap, fixed minimally here because Task 6's JSON manifests
  # carry full party names ("Democratic Party") rather than abbreviations,
  # and rcvr_PARTY_PATTERNS only lists abbreviations. Exact (not prefix)
  # lookup against rcvr_PARTY_FULL_NAMES, on the party field only (never
  # candidate, so a surname is never mistaken for a party name); anything
  # not an exact match ("Democratic-Republican Party") falls through and
  # keeps its raw value below, same as today.
  full <- unname(rcvr_PARTY_FULL_NAMES[normalize_party_full_name(party)])
  hit <- !is.na(party) & is.na(out) & !is.na(full)
  out[hit] <- full[hit]

  # a party abbreviation that matched nothing above is kept as written
  out <- dplyr::coalesce(out, party)

  # non-candidates carry no party
  blank <- !is.na(candidate) &
    stringr::str_detect(candidate, stringr::regex("undervote|overvote|No image found", TRUE))
  out[blank] <- NA_character_

  out
}

#' Seed a cleaned `candidate` value from the raw candidate string
#'
#' Removes party abbreviations, parenthetical suffixes, non-marking unicode
#' (diacritics), and stray `NA`/`N/A` placeholders, then squishes whitespace.
#' Write-ins collapse to `"WI"`. This is a **seed**, not a rule.
#'
#' @param candidate Raw candidate string.
#'
#' @return A cleaned character vector.
seed_candidate <- function(candidate) {
  out <- candidate |>
    stringr::str_remove_all("^$|^NA$|^N/A$|\\([^)]*\\)$|[\\p{Mn}]") |>
    stringi::stri_trans_nfd()

  all_abbrev <- c(unlist(rcvr_PARTY_PATTERNS, use.names = FALSE), "No image found")

  out <- out |>
    stringr::str_remove_all(stringr::regex(gen_patterns(all_abbrev), TRUE)) |>
    stringr::str_remove_all("^$|^NA$|^N/A$|\\([^)]*\\)$|[\\p{Mn}]") |>
    stringr::str_squish()

  ifelse(stringr::str_detect(out, stringr::regex("Write", TRUE)), "WI", out)
}

# used for detecting merged rows and filling in magnitude
find_consecutive_ranges <- function(indices) {
  breaks <- which(diff(indices) > 1)
  starts <- c(1, breaks + 1)
  ends <- c(breaks, length(indices))

  ranges <- list()
  for (i in seq_along(starts)) {
    ranges[[i]] <- indices[starts[i]:ends[i]]
  }
  ranges
}

# infer magnitude from runs of `...N` placeholder contest columns, which is
# how multi-seat contests appear in delimited exports
infer_magnitude_delim <- function(meta) {
  if (!isTRUE(any(meta$magnitude > 0, na.rm = TRUE))) {
    return(meta)
  }

  cs <- dplyr::pull(meta, contest)
  cs_ind <- which(stringr::str_detect(cs, "^\\.\\.\\.\\d+$"))
  if (length(cs_ind) == 0) return(meta)

  ranges <- find_consecutive_ranges(cs_ind)
  mags <- rep.int(0, length(cs))

  for (r in ranges) {
    mags[c(r[1] - 1, r)] <- cs[c(r[1] - 1, r)] |> unique() |> length()
  }

  meta |>
    dplyr::mutate(magnitude2 = dplyr::na_if(mags, 0)) |>
    dplyr::group_by(contest) |>
    tidyr::fill(magnitude2, .direction = "up") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      magnitude2 = tidyr::replace_na(magnitude2, 1),
      magnitude = dplyr::coalesce(magnitude, as.integer(magnitude2)),
      magnitude2 = NULL
    )
}

#' Seed metadata rows for a county
#'
#' Runs the regex-based cleaning that produces a first guess at `candidate`,
#' `party_detailed`, `district`, and `magnitude` for every raw
#' (contest, candidate) pair. These are **seeds**: once a pair exists in the
#' store, the store owns its values and this function's output is discarded.
#'
#' @param pairs A pairs frame from one of the `read_*_cvr()` readers.
#' @param type The resolved reader type: `"DELIM"`, `"DELIM-MULTI"`, `"JSON"`,
#'   `"XML"`, or `"SPECIAL"`.
#' @param path Path to the CVR, used by the JSON branch to read manifests.
#' @param election The election label, e.g. `"2020 General"`.
#' @param state The state name, upper case.
#' @param county The county name, upper case.
#' @param verbose Verbose output? Default `FALSE`.
#'
#' @return A tibble with the columns in `rcvr_SEED_COLS`, one row per raw
#'   (contest, candidate) pair.
gen_metadata <- function(pairs, type, path, election, state, county, verbose = FALSE) {
  key_args <- list(election = election, state = state, county = county)
  bad <- names(key_args)[
    !vapply(key_args, checkmate::test_string, logical(1), na.ok = FALSE)
  ]
  if (length(bad) > 0) {
    cli::cli_abort(
      "{.arg {bad}} must be a single non-NA string.",
      class = "rcvr_bad_metadata_args"
    )
  }
  assert_pairs(pairs)

  if (verbose) cli::cli_alert_info("Generating metadata for {.val {type}} CVR format")

  meta <- pairs |>
    dplyr::filter(!(contest %in% c(rcvr_DROP_COLS, rcvr_RENAME_COLS))) |>
    dplyr::distinct(contest, raw_candidate, raw_party) |>
    dplyr::mutate("ballot_order" = dplyr::cur_group_id(), .by = "contest") |>
    dplyr::mutate(
      office = NA_character_,
      district = NA_character_,
      candidate = seed_candidate(raw_candidate),
      party_detailed = seed_party(raw_party, raw_candidate),
      # a nonpartisan row carries BOTH the party value and the flag; NA only
      # when no party could be determined at all
      nonpartisan = dplyr::if_else(
        is.na(party_detailed), NA, party_detailed == "NONPARTISAN"
      ),
      magnitude = stringr::str_extract(
        contest, stringr::regex("Vote For.*?(\\d+)", TRUE), group = 1
      ) |> as.integer(),
      drop = FALSE
    ) |>
    dplyr::filter(
      !stringr::str_detect(candidate, stringr::regex("^undervote$|^overvote$|^WI$|^0$", TRUE))
    ) |>
    dplyr::arrange(ballot_order)

  if (type %in% c("DELIM", "DELIM-MULTI")) {
    meta <- infer_magnitude_delim(meta)
  } else if (type == "JSON") {
    # district and magnitude come from the Dominion manifests, not from regex
    ctx <- json_seed_context(path)
    meta <- meta |>
      dplyr::select(-district, -magnitude) |>
      dplyr::left_join(ctx, dplyr::join_by(contest))
  }

  meta |>
    dplyr::mutate(
      election = election,
      state = state,
      county = county
    ) |>
    dplyr::select(tidyselect::all_of(rcvr_SEED_COLS))
}
