#' Merge ballots that a vendor split across several rows (multi-card ballots)
#'
#' Some exports write one row per ballot *card*: a two-card ballot is two
#' consecutive rows with the same ballot style, the first holding the card-1
#' contests and the second the card-2 contests. A contest that is on a card
#' but not voted is written as `"0"`; a contest that is not on that card is
#' blank. So within one ballot style, card rows have complementary coverage.
#'
#' Detection is per ballot style (with any `" [n]"` card suffix stripped):
#' take the style's most-filled contest column as the anchor; columns filled
#' (almost) only on anchor rows are card-1 columns, columns filled (almost)
#' only on non-anchor rows are continuation columns. A row with nothing in
#' the card-1 columns and something in the continuation columns is a
#' continuation and is merged into the row just above it, provided that row
#' has the same style. Ballot styles whose contests simply differ (one
#' party's primary, a city's local races) never split *within* a style, so
#' they are left alone.
#'
#' @param raw A wide CVR tibble.
#' @param path The source path, used only in the warning message.
#'
#' @return `raw`, with continuation rows merged into their ballot.
fix_fragmentation <- function(raw, path = NA) {
  if (!("ballot_style" %in% colnames(raw)) || nrow(raw) < 2) {
    return(raw)
  }

  key <- c(rcvr_DROP_COLS, unname(rcvr_RENAME_COLS), names(rcvr_RENAME_COLS),
           "cvr_id", "ballot_style2")
  contest_cols <- setdiff(colnames(raw), key)
  if (length(contest_cols) < 2) return(raw)

  filled <- vapply(raw[contest_cols], function(x) !is.na(x) & x != "",
                   logical(nrow(raw)))
  if (is.null(dim(filled))) filled <- matrix(filled, nrow = 1)
  style <- stringr::str_remove(as.character(raw$ballot_style), " \\[\\d+\\]$")
  style[is.na(style)] <- ""

  cont <- logical(nrow(raw))
  for (s in unique(style)) {
    rows <- which(style == s)
    if (length(rows) < 2) next
    f <- filled[rows, , drop = FALSE]
    # card-1 contests come first in column order, so the first contest used
    # by this style is on card 1 (the most-filled column is not: card-1 and
    # card-2 columns are each filled on about half the rows)
    anchor <- which(colSums(f) > 0)[1]
    if (is.na(anchor)) next
    a <- f[, anchor]
    if (all(a) || !any(a)) next
    rate_a <- colMeans(f[a, , drop = FALSE])
    rate_b <- colMeans(f[!a, , drop = FALSE])
    card1 <- rate_a > 0 & rate_b < 0.01
    card2 <- rate_b > 0 & rate_a < 0.01
    if (!any(card2)) next
    # the non-anchor rows must be (almost) entirely continuation cards; a
    # style where they mostly carry card-1 contests is not multi-card
    fills_b <- sum(f[!a, , drop = FALSE])
    if (fills_b == 0 || sum(f[!a, card2, drop = FALSE]) / fills_b < 0.9) next
    is_cont <- rowSums(f[, card1, drop = FALSE]) == 0 & rowSums(f[, card2, drop = FALSE]) > 0
    cont[rows[is_cont]] <- TRUE
  }

  if (!any(cont)) return(raw)

  # A continuation card can only be linked to its voter when the export keeps
  # each voter's cards together: card 2 directly after card 1, same style.
  # Election-day tabulators often take cards in arbitrary order (Clark OH
  # 2024), and then no linkage exists in the data at all -- merging there
  # would attach cards to the wrong voters. So link batch by batch, and only
  # in batches where (almost) every continuation follows a main row of the
  # same style.
  prev_ok <- c(FALSE, style[-1] == style[-length(style)] & !cont[-length(cont)])
  batch_cols <- intersect(c("TabulatorNum", "Tabulator Num", "TabulatorId",
                            "BatchId", "Batch Id", "Batch"), colnames(raw))
  batch <- if (length(batch_cols) > 0) {
    do.call(paste, c(lapply(raw[batch_cols], as.character), sep = "\r"))
  } else {
    rep("", nrow(raw))
  }
  share_ok <- tapply(prev_ok[cont], batch[cont], mean)
  linked_batch <- names(share_ok)[share_ok >= 0.98]
  unlinked <- cont & !(batch %in% linked_batch)
  cont <- cont & prev_ok & batch %in% linked_batch

  if (any(unlinked)) {
    cli::cli_warn(
      c("{.file {as.character(path)}}: {sum(unlinked)} continuation card row{?s} cannot be linked to a voter (cards out of order within their batch); each is kept as its own ballot row.",
        "i" = "For these, {.field cvr_id} identifies a card, not a voter."),
      class = "rcvr_unlinked_cards"
    )
  }
  if (!any(cont)) return(raw)

  cli::cli_warn(
    "{.file {as.character(path)}}: merged {sum(cont)} continuation card row{?s} into the ballot row above (multi-card ballots)."
  )

  grp <- cumsum(!cont)
  main_row <- which(!cont)[grp]
  for (col in contest_cols) {
    v <- raw[[col]]
    take <- cont & filled[, col]
    if (any(take)) {
      tgt <- main_row[take]
      blank_tgt <- is.na(v[tgt]) | v[tgt] == ""
      v[tgt[blank_tgt]] <- v[take][blank_tgt]
      raw[[col]] <- v
    }
  }
  raw[!cont, , drop = FALSE]
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
