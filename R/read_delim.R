#' Merge ballots that a vendor fragmented across multiple rows
#'
#' @param raw A wide CVR tibble.
#' @param path The source path, used only in the warning message.
#'
#' @return `raw`, with fragmented rows merged where detected.
fix_fragmentation <- function(raw, path = NA) {
  if (!("ballot_style" %in% colnames(raw))) {
    return(raw)
  }

  foundbrackets <- dplyr::distinct(raw, ballot_style) |>
    dplyr::pull() |>
    stringr::str_detect("\\[\\d+\\]$") |>
    sum() > 0

  firstcol <- setdiff(
    colnames(raw),
    c(rcvr_DROP_COLS, "cvr_id", "precinct", "ballot_style")
  )[1]

  foundspaces <- raw[[firstcol]] |>
    table() |>
    as.data.frame() |>
    dplyr::mutate(p = Freq / sum(Freq)) |>
    dplyr::filter(Var1 == "") |>
    dplyr::pull(p) > 0.1

  if (!(isTRUE(foundbrackets) | isTRUE(foundspaces))) {
    return(raw)
  }

  cli::cli_warn(
    "{.file {path}} appears to contain fragmented ballots (where the ballot is split across multiple rows). We attempt to repair this by merging rows together using an intelligent strategy, but any further analysis should be taken with caution."
  )

  tokeep <- raw[[firstcol]] != ""

  base <- raw |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), ~ dplyr::na_if(.x, "")),
      ballot_style2 = stringr::str_remove(ballot_style, " \\[\\d+\\]$"),
      .before = ballot_style
    ) |>
    dplyr::group_by(ballot_style2) |>
    tidyr::fill(tidyselect::everything(), .direction = "downup") |>
    dplyr::ungroup()

  base[tokeep, ]
}

#' Read a single delimited CVR file
#'
#' @param path Path to a CSV, XLS, or XLSX CVR.
#'
#' @return A wide tibble, one row per ballot, with `precinct` and
#'   `ballot_style` present (possibly all-`NA`).
#' @export
read_delim_cvr <- function(path) {
  path <- fs::path_real(path)

  # checked ahead of is_header(), which assumes a csv/xls/xlsx extension and
  # otherwise leaves its own `d` undefined -- an unrecognised extension must
  # produce this abort, not a bare "object 'd' not found"
  ext <- fs::path_ext(path) |> stringr::str_to_upper()
  if (!(ext %in% c("CSV", "XLS", "XLSX"))) {
    cli::cli_abort("{.file {path}} is not a CSV or Excel file")
  }

  if (is_header(path)) {
    raw <- header_processor(path)
  } else {
    raw <- switch(ext,
      "CSV" = data.table::fread(path, colClasses = "character", header = TRUE),
      "XLS" = readxl::read_excel(path, col_types = "text", .name_repair = "unique_quiet"),
      "XLSX" = readxl::read_excel(path, col_types = "text", .name_repair = "unique_quiet")
    )
  }

  colnames(raw) <- iconv(colnames(raw), to = "UTF-8", sub = "")

  # Select one matching candidate for each canonical name defined in
  # `rcvr_RENAME_COLS`. We pick the first candidate that appears in the
  # file (priority determined by the ordering in `rcvr_RENAME_COLS`). This
  # ensures only one `ballot_style` column and one `precinct` column are
  # chosen when multiple variants exist.
  for (canonical in unique(names(rcvr_RENAME_COLS))) {
    candidates <- rcvr_RENAME_COLS[names(rcvr_RENAME_COLS) == canonical]
    found <- candidates[candidates %in% colnames(raw)]
    if (length(found) > 0) {
      old_name <- found[1]
      if (old_name != canonical) {
        colnames(raw)[colnames(raw) == old_name] <- canonical
      }
    } else {
      raw[[canonical]] <- NA_character_
    }
  }

  # after the canonicalisation loop, so a canonical rename is in place and can
  # never be mistaken for a placeholder, and so a placeholder run can never
  # inherit a key column's name
  resolved <- resolve_placeholder_cols(colnames(raw))
  colnames(raw) <- resolved$names

  raw <- tibble::as_tibble(raw)
  attr(raw, "rcvr_magnitude") <- resolved$magnitude
  raw
}

#' Read a directory of delimited CVR files
#'
#' @param dir Path to a directory of CSV or Excel CVRs.
#'
#' @return A wide tibble, all files row-bound.
#' @export
read_delim_multi_cvr <- function(dir) {
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  # `read_delim_multi_cvr()` is public and callable directly, so it must be
  # safe to point at a directory holding a stray non-delim file (a JSON
  # note, a `Write In Images` subfolder) without aborting the whole county.
  delim_files <- files[stringr::str_detect(files, "\\.(csv|CSV|xls|XLS|xlsx|XLSX)$")]

  if (length(delim_files) == 0) {
    cli::cli_abort(
      "No delimited files found in {.file {dir}}.",
      class = "rcvr_no_files"
    )
  }

  raws <- lapply(delim_files, read_delim_cvr)

  out <- dplyr::bind_rows(raws)

  # `bind_rows()` drops the attribute, and two files in one directory may each
  # contribute a magnitude for the same contest. Take the widest run seen.
  mags <- unlist(lapply(raws, attr, "rcvr_magnitude"))
  attr(out, "rcvr_magnitude") <- if (length(mags) > 0) {
    vapply(split(as.integer(mags), names(mags)), max, integer(1))
  } else {
    integer(0)
  }

  out
}

#' Reshape a wide delimited CVR into the pairs frame
#'
#' @param raw A tibble from [read_delim_cvr()] or [read_delim_multi_cvr()].
#' @param path The source path, used only in warning messages.
#'
#' @return A pairs frame: `cvr_id`, `precinct`, `contest`, `raw_candidate`,
#'   `raw_party`, `rank`.
pairs_from_delim <- function(raw, path = NA) {
  # the reader recorded one magnitude per multi-column contest; dplyr drops
  # the attribute, so carry it across by hand onto the returned pairs frame,
  # where gen_metadata() picks it up
  magnitude <- attr(raw, "rcvr_magnitude")

  raw <- fix_fragmentation(raw, path) |>
    dplyr::select(-tidyselect::any_of(c("ballot_style", "ballot_style2")))

  pairs <- raw |>
    dplyr::select(-tidyselect::any_of(rcvr_DROP_COLS)) |>
    dplyr::mutate("cvr_id" = seq_len(dplyr::n())) |>
    tidyr::pivot_longer(
      cols = -tidyselect::any_of(c("cvr_id", "precinct")),
      names_to = "contest",
      values_to = "raw_candidate",
      values_drop_na = TRUE,
      values_transform = as.character
    ) |>
    dplyr::mutate(
      contest = stringr::str_remove(contest, stringr::regex("^Choice_\\d+_\\d+:", TRUE)),
      # strip the dedup marker `header_processor()` appended (rcvr_DUP_SENTINEL)
      # before separate_wider_delim() can push it onto the last component
      contest = stringr::str_remove(contest, "__RCVRDUP__\\d+$")
    ) |>
    tidyr::separate_wider_delim(
      cols = contest,
      delim = stringr::regex("\\|\\||:"),
      names = c("contest", "candidate", "raw_party"),
      too_few = "align_start"
    ) |>
    # some rows are aggregated values (CvrNumber=`Redacted and Aggregated...`),
    # need to drop these rows
    dplyr::filter(
      suppressWarnings(as.numeric(raw_candidate)) <= 1 | is.na(suppressWarnings(as.numeric(raw_candidate))),
      contest != raw_candidate
    ) |>
    dplyr::filter(raw_candidate != "0", raw_candidate != "") |>
    dplyr::group_by(precinct) |>
    tidyr::complete(cvr_id, tidyr::nesting(contest)) |>
    dplyr::ungroup() |>
    # if they voted, we use their lookup-table candidate choice; if they didn't,
    # they get assigned to undervote. This also deals with CVRs that have
    # contests as columns and candidates as cells.
    dplyr::mutate(
      candidate = as.character(candidate),
      raw_candidate = dplyr::case_when(
        raw_candidate %in% rcvr_REDACT_NAMES ~ "redacted",
        is.na(raw_candidate) ~ "undervote",
        .default = dplyr::coalesce(candidate, as.character(raw_candidate))
      ),
      candidate = NULL,
      cvr_id = as.integer(cvr_id),
      rank = NA_integer_,
      dplyr::across(
        c(contest, raw_candidate, raw_party),
        ~ dplyr::na_if(.x, "")
      )
    ) |>
    dplyr::select(cvr_id, precinct, contest, raw_candidate, raw_party, rank)

  attr(pairs, "rcvr_magnitude") <- magnitude

  assert_pairs(pairs)
}
