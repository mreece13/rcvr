# Sparse form of a wide delimited CVR.
#
# A wide export is mostly empty or "0" cells: Bergen NJ 2024 has 7,670
# columns and about 1,665 filled cells per row, 99% of them "0". Reading such
# a file whole needs tens of GB, so everything after the reader works on a
# sparse form instead:
#
# * `key`: one row per file row -- precinct, ballot style, tabulator batch,
#   record id, whether the row has any key cell, whether it has any "0";
# * `votes`: one row per cell holding a vote (non-blank, not "0"), with its
#   file row and its column (an index into the layout's value columns);
# * `fill`: one row per (file row, contest) where any of the contest's
#   columns is non-blank, "0" included. A "0" means "on this card, not
#   voted", a blank "not on this card", which is what multi-card linking
#   needs.
#
# The same form is built from a whole wide frame (`delim_cells()`) or chunk
# by chunk from a large CSV (`read_delim_chunked()`), and turned into pairs
# by one function (`pairs_from_cells()`).

# names that are never contests
delim_key_names <- function() {
  c(rcvr_DROP_COLS, unname(rcvr_RENAME_COLS), unique(names(rcvr_RENAME_COLS)),
    "cvr_id", "ballot_style2")
}

# How a wide frame's columns are read: which are contests, how each splits
# into contest/candidate/party/rank, and which hold the batch and record id.
delim_layout <- function(nms) {
  # a dedup sentinel never changes what a column is: a repeated key column is
  # still a key column, and a seat column still belongs to its contest
  bare <- stringr::str_remove(nms, paste0(rcvr_DUP_SENTINEL, "\\d+$"))
  is_value <- !(bare %in% delim_key_names())
  value_cols <- which(is_value)
  info <- parse_contest_cols(bare[value_cols])
  list(
    names = nms,
    value_cols = value_cols,
    info = info,
    # fill group of each value column, numbered in column order, so the
    # lowest id a ballot style uses is its first contest. In a
    # candidate-per-column layout every column of a contest is filled alike
    # ("0" or a mark), so the contest is the group and the fill table stays
    # small (Bergen NJ: ~1,665 filled cells but ~40 contests per row). In a
    # contest-per-column layout each column is its own group, as a seat
    # column may be blank when the voter used fewer seats.
    cid = {
      g <- ifelse(is.na(info$candidate), paste0("\r", seq_along(value_cols)), info$contest)
      match(g, unique(g))
    },
    key_cols = which(!is_value & !(bare %in% c("precinct", "ballot_style", "ballot_style2"))),
    batch_cols = intersect(c("TabulatorNum", "Tabulator Num", "TabulatorId",
                             "BatchId", "Batch Id", "Batch"), nms),
    rec_col = intersect(c("RecordId", "Record Id"), nms)[1],
    has_style = "ballot_style" %in% nms
  )
}

# Build the sparse form of a wide frame (or one chunk of a file, whose first
# row is file row `offset + 1`).
delim_cells <- function(raw, layout, offset = 0L) {
  n <- nrow(raw)
  chr <- function(x) if (is.character(x)) x else as.character(x)
  col_or_na <- function(nm) {
    if (!is.na(nm) && nm %in% colnames(raw)) chr(raw[[nm]]) else rep(NA_character_, n)
  }

  has_key <- logical(n)
  for (j in layout$key_cols) {
    x <- chr(raw[[j]])
    has_key <- has_key | (!is.na(x) & x != "")
  }
  batch <- if (length(layout$batch_cols) > 0) {
    do.call(paste, c(lapply(layout$batch_cols, function(b) as.character(raw[[b]])), sep = "\r"))
  } else {
    rep("", n)
  }

  has_zero <- logical(n)
  nv <- length(layout$value_cols)
  vrow <- vector("list", nv)
  vval <- vector("list", nv)
  groups <- split(seq_len(nv), layout$cid)
  frow <- vector("list", length(groups))
  for (g in seq_along(groups)) {
    f <- logical(n)
    for (k in groups[[g]]) {
      x <- chr(raw[[layout$value_cols[k]]])
      nb <- !is.na(x) & x != ""
      zero <- nb & x == "0"
      f <- f | nb
      has_zero <- has_zero | zero
      keep <- which(nb & !zero)
      vrow[[k]] <- keep
      vval[[k]] <- x[keep]
    }
    frow[[g]] <- which(f)
  }
  offset <- as.integer(offset)

  list(
    n = n,
    key = data.table::data.table(
      precinct = col_or_na("precinct"),
      style = col_or_na(if (layout$has_style) "ballot_style" else NA),
      batch = batch,
      rec = col_or_na(layout$rec_col),
      has_key = has_key,
      has_zero = has_zero
    ),
    votes = data.table::data.table(
      row = unlist(vrow, use.names = FALSE) + offset,
      col = rep.int(seq_len(nv), lengths(vrow)),
      value = as.character(unlist(vval, use.names = FALSE))
    ),
    fill = data.table::data.table(
      row = unlist(frow, use.names = FALSE) + offset,
      cid = rep.int(as.integer(names(groups)), lengths(frow))
    )
  )
}

# Row-bind the sparse forms of consecutive chunks (already offset).
bind_cells <- function(parts) {
  list(
    n = sum(vapply(parts, `[[`, integer(1), "n")),
    key = data.table::rbindlist(lapply(parts, `[[`, "key")),
    votes = data.table::rbindlist(lapply(parts, `[[`, "votes")),
    fill = data.table::rbindlist(lapply(parts, `[[`, "fill"))
  )
}

# Drop file rows (repeated header blocks) and renumber the rest.
drop_cell_rows <- function(cells, drop) {
  drop <- unique(drop[drop >= 1 & drop <= cells$n])
  if (length(drop) == 0) return(cells)
  keep <- rep(TRUE, cells$n)
  keep[drop] <- FALSE
  new <- cumsum(keep)
  votes <- cells$votes[keep[row]]
  votes[, row := new[row]]
  fill <- cells$fill[keep[row]]
  fill[, row := new[row]]
  list(n = sum(keep), key = cells$key[keep], votes = votes, fill = fill)
}

#' Decide which card rows belong to one voter (multi-card ballots)
#'
#' Some exports write one row per ballot *card*: a two-card ballot is two
#' consecutive rows with the same ballot style, the first holding the card-1
#' contests and the second the card-2 contests. A contest that is on a card
#' but not voted is written as `"0"`; a contest that is not on that card is
#' blank. So within one ballot style, card rows have complementary coverage.
#'
#' Detection is per ballot style (with any `" [n]"` card suffix stripped):
#' the style's first contest in column order is the anchor; contests filled
#' (almost) only on anchor rows are card-1 contests, contests filled (almost)
#' only on non-anchor rows are continuation contests. A row with nothing in
#' the card-1 contests and something in the continuation contests is a
#' continuation and is merged into the row just above it, provided that row
#' has the same style. Ballot styles whose contests simply differ (one
#' party's primary, a city's local races) never split *within* a style, so
#' they are left alone.
#'
#' @param cells The sparse form from `delim_cells()`.
#' @param path The source path, used only in warning messages.
#'
#' @return `NULL` when no row is a continuation, otherwise a list: `o`, the
#'   row order to link in; `linked`, which rows (in that order) merge into
#'   the row above; `unlinked`, which continuation rows could not be linked.
#' @noRd
fragment_links <- function(cells, path = NA) {
  n <- cells$n
  if (n < 2) return(NULL)
  # no ballot style in the file (Salt Lake UT 2025 non-RCV export): the
  # synthesised all-NA column would make the whole file one "style", and the
  # anchor rule then calls most single-card ballots continuations
  if (all(is.na(cells$key$style) | cells$key$style == "")) return(NULL)
  style <- stringr::str_remove(cells$key$style, " \\[\\d+\\]$")
  style[is.na(style)] <- ""

  f <- data.table::data.table(row = cells$fill$row, cid = cells$fill$cid)
  if (nrow(f) == 0) return(NULL)
  f[, s := style[row]]
  n_s <- data.table::data.table(s = style)[, list(n = .N), by = "s"]
  f <- f[n_s[n >= 2], on = "s", nomatch = NULL]
  if (nrow(f) == 0) return(NULL)

  # card-1 contests come first in column order, so the first contest used by
  # this style is on card 1 (the most-filled contest is not: card-1 and
  # card-2 contests are each filled on about half the rows)
  f[, anchor := min(cid), by = "s"]
  a <- logical(n)
  a[f[cid == anchor, row]] <- TRUE
  f[, in_a := a[row]]

  st <- data.table::data.table(s = style, a = a)[, list(n_a = sum(a), n_b = sum(!a)), by = "s"]
  st <- st[n_a > 0 & n_b > 0]
  cnt <- f[, list(ca = sum(in_a), cb = sum(!in_a)), by = c("s", "cid")]
  cnt <- cnt[st, on = "s", nomatch = NULL]
  # a card-2 contest may also sit on a few single-sheet ballots that carry
  # every contest (remade ballots: 1.2% of Pueblo CO 2022's style 9), so it
  # is allowed on up to 5% of anchor rows
  cnt[, `:=`(card1 = ca > 0 & cb / n_b < 0.01, card2 = cb > 0 & ca / n_a < 0.05)]

  # the non-anchor rows must be (almost) entirely continuation cards; a style
  # where they mostly carry card-1 contests is not multi-card
  ok <- cnt[, list(ok = any(card2) && sum(cb) > 0 && sum(cb[card2]) / sum(cb) >= 0.9), by = "s"][ok == TRUE, s]
  if (length(ok) == 0) return(NULL)
  cnt <- cnt[s %in% ok]

  f <- f[cnt[, c("s", "cid", "card1", "card2")], on = c("s", "cid"), nomatch = NULL]
  per_row <- f[, list(c1 = sum(card1), c2 = sum(card2)), by = "row"]
  cont <- logical(n)
  cont[per_row[c1 == 0 & c2 > 0, row]] <- TRUE
  if (!any(cont)) return(NULL)

  # A continuation card can only be linked to its voter when the export keeps
  # each voter's cards together: card 2 directly after card 1, same style.
  # Election-day tabulators often take cards in arbitrary order (Clark OH
  # 2024), and then no linkage exists in the data at all -- merging there
  # would attach cards to the wrong voters. So link batch by batch, and only
  # in batches where (almost) every continuation follows a main row of the
  # same style.
  batch <- cells$key$batch
  link <- function(o) {
    s <- style[o]
    k <- cont[o]
    b <- batch[o]
    prev_ok <- c(FALSE, s[-1] == s[-length(s)] & !k[-length(k)])
    share_ok <- tapply(prev_ok[k], b[k], mean)
    linked_batch <- names(share_ok)[share_ok >= 0.98]
    list(
      o = o,
      linked = k & prev_ok & b %in% linked_batch,
      unlinked = k & !(b %in% linked_batch)
    )
  }
  res <- link(seq_len(n))

  # Some exports are sorted by precinct rather than scan order (Pueblo CO
  # 2024), so a voter's cards are not adjacent rows even though they were
  # scanned back to back. Scan order survives in the record id within each
  # tabulator batch; when linking in file order leaves cards unlinked, try
  # that order too and keep whichever links more. The same >= 98% per-batch
  # rule applies, so a batch whose cards were genuinely shuffled still stays
  # unlinked.
  if (any(res$unlinked) && !all(is.na(cells$key$rec)) && any(batch != "")) {
    # Riverside CA writes key cells as Excel text formulas: ="17"
    rec <- suppressWarnings(as.numeric(sub('^="(.*)"$', "\\1", cells$key$rec)))
    if (!anyNA(rec)) {
      alt <- link(order(batch, rec))
      if (sum(alt$linked) > sum(res$linked)) res <- alt
    }
  }

  # cli's {.file} errors on NA, and `path` is NA when called without one
  src <- if (length(path) == 1 && !is.na(path)) as.character(path) else "CVR"
  if (any(res$unlinked)) {
    cli::cli_warn(
      c("{.file {src}}: {sum(res$unlinked)} continuation card row{?s} cannot be linked to a voter (cards out of order within their batch); each is kept as its own ballot row.",
        "i" = "For these, {.field cvr_id} identifies a card, not a voter."),
      class = "rcvr_unlinked_cards"
    )
  }
  if (any(res$linked)) {
    cli::cli_warn(
      "{.file {src}}: merged {sum(res$linked)} continuation card row{?s} into the ballot row above (multi-card ballots)."
    )
  }
  res
}

# Turn the sparse form into the pairs frame.
pairs_from_cells <- function(cells, layout, magnitude = integer(0), path = NA) {
  n <- cells$n
  key <- cells$key
  votes <- cells$votes
  info <- layout$info

  # merge linked continuation cards into the ballot above: `ballot` maps
  # every file row to its output cvr_id, in linking order
  fr <- if (layout$has_style) fragment_links(cells, path) else NULL
  ballot <- seq_len(n)
  main_of <- seq_len(n)
  if (!is.null(fr) && any(fr$linked)) {
    grp <- cumsum(!fr$linked)
    ballot[fr$o] <- grp
    main_of[fr$o] <- fr$o[which(!fr$linked)][grp]

    # a continuation's contest is taken only where the main card has none of
    # it (it never does when the cards are complementary)
    cont_row <- main_of != seq_len(n)
    cv <- votes[cont_row[row]]
    if (nrow(cv) > 0) {
      cv[, `:=`(m = main_of[row], cid = layout$cid[col])]
      main_fill <- cells$fill[!cont_row[row], list(row, cid)]
      clash <- cv[main_fill, on = c(m = "row", "cid"), which = TRUE, nomatch = NULL]
      if (length(clash) > 0) {
        votes <- votes[!cv[unique(clash), list(row, col)], on = c("row", "col")]
      }
    }
  }
  nb <- max(ballot, 0L)

  # one key per output ballot, the main card's; a ballot has a "0" if any of
  # its cards does
  mains <- which(main_of == seq_len(n))
  mains <- mains[order(ballot[mains])]
  precinct <- key$precinct[mains]
  has_key <- key$has_key[mains]
  has_zero <- logical(nb)
  has_zero[ballot[key$has_zero]] <- TRUE

  cvr_id <- ballot[votes$row]
  value <- votes$value
  col <- votes$col

  # some rows are aggregated counts (CvrNumber = `Redacted and Aggregated...`),
  # and some exports repeat the header partway through (a cell equal to its
  # own contest's name); neither is a vote
  u <- unique(value)
  num <- suppressWarnings(as.numeric(u))
  aggregated <- u[!is.na(num) & num > 1]
  junk <- value %in% aggregated | value == info$contest[col]
  junk[is.na(junk)] <- FALSE
  junk_rows <- unique(cvr_id[junk])
  cvr_id <- cvr_id[!junk]; value <- value[!junk]; col <- col[!junk]

  # if they voted, use the column's candidate (candidate-per-column layout);
  # otherwise the cell is the candidate (contest-per-column layout)
  cand <- info$candidate[col]
  raw_candidate <- dplyr::case_when(
    value %in% rcvr_REDACT_NAMES ~ "redacted",
    .default = dplyr::coalesce(cand, value)
  )

  votes <- data.table::data.table(
    cvr_id = cvr_id,
    precinct = precinct[cvr_id],
    contest = info$contest[col],
    raw_candidate = raw_candidate,
    raw_party = info$party[col],
    rank = info$rank[col],
    ord = col
  )

  # A ballot gets an undervote for each contest seen in its precinct that it
  # cast no vote in. Every ballot row counts, including one with no marks at
  # all (Georgia, Sauk WI) -- unless the row only held aggregated counts or a
  # repeated header, which are not ballots, or the row is entirely empty.
  # A row is a ballot if it carries an id or other key cell, or a "0".
  is_ballot <- has_key | has_zero
  is_ballot[setdiff(junk_rows, cvr_id)] <- FALSE
  is_ballot[unique(cvr_id)] <- TRUE

  ballots <- data.table::data.table(cvr_id = which(is_ballot), precinct = precinct[is_ballot])
  first_ord <- votes[, list(ord = min(ord)), by = "contest"]
  pc <- unique(votes[, c("precinct", "contest")])
  grid <- ballots[pc, on = "precinct", allow.cartesian = TRUE, nomatch = NULL]
  # A ballot with no precinct would join every contest in the file (all NA
  # precincts form one group). For those, the contests on the ballot are the
  # ones with a filled cell ("0" = on the ballot, not voted).
  #
  # The same holds for every ballot when the vendor writes its own
  # undervotes (an "undervote" cell, Manatee FL 2020): a blank cell then
  # means the contest is not on that card, not that it went unvoted. Without
  # a ballot style the cards of one voter cannot be linked, so a precinct
  # grid would give each card an undervote for every contest on the voter's
  # other cards (Manatee: 216,841 president undervotes on second cards).
  explicit_under <- any(stringr::str_to_lower(value) == "undervote", na.rm = TRUE)
  scoped <- is.na(ballots$precinct) | explicit_under
  if (any(scoped)) {
    cmap <- unique(data.table::data.table(cid = layout$cid, contest = info$contest))
    fb <- cells$fill[, list(cvr_id = ballot[row], cid)]
    fb <- unique(cmap[fb, on = "cid", nomatch = NULL][, c("cvr_id", "contest")])
    fb <- fb[cvr_id %in% ballots$cvr_id[scoped] & contest %in% pc$contest]
    data.table::set(fb, j = "precinct", value = precinct[fb$cvr_id])
    grid <- data.table::rbindlist(
      list(grid[!(cvr_id %in% ballots$cvr_id[scoped])], fb), use.names = TRUE
    )
  }
  voted <- unique(votes[, c("cvr_id", "contest")])
  under <- grid[!voted, on = c("cvr_id", "contest")]
  under <- first_ord[under, on = "contest"]
  under[, `:=`(raw_candidate = "undervote", raw_party = NA_character_, rank = NA_integer_)]

  pairs <- data.table::rbindlist(list(votes, under), use.names = TRUE)
  data.table::setorderv(pairs, c("cvr_id", "ord"))
  pairs[, ord := NULL]
  for (v in c("contest", "raw_candidate", "raw_party")) {
    data.table::set(pairs, which(pairs[[v]] == ""), v, NA_character_)
  }
  pairs[, cvr_id := as.integer(cvr_id)]

  pairs <- tibble::as_tibble(pairs)
  attr(pairs, "rcvr_magnitude") <- magnitude

  assert_pairs(pairs)
}

#' Read a large delimited CVR in row chunks
#'
#' A wide CSV is read whole by `read_delim_cvr()`, which costs about
#' 8 bytes per cell: Bergen NJ 2024 (11 GB, 7,670 columns) would need
#' tens of GB. This reads the header with `read_delim_cvr(n = )`, then
#' streams the ballot rows `chunk_cells` cells at a time, keeping only the
#' sparse form of each chunk. The result is the same pairs frame as
#' `read_delim_cvr(path) |> pairs_from_delim(path)`.
#'
#' `read_cvr()` uses this for a CSV larger than
#' `getOption("rcvr.chunk_bytes", 1e9)` bytes. Rows are split at line ends,
#' so a quoted cell holding a line break is not supported.
#'
#' @param path Path to a CSV.
#' @param chunk_cells Cells to read per chunk.
#'
#' @return A pairs frame.
#' @noRd
read_delim_chunked <- function(path, chunk_cells = getOption("rcvr.chunk_cells", 2e7)) {
  path <- fs::path_real(path)
  proto <- read_delim_cvr(path, n = 10)
  skip <- attr(proto, "rcvr_skip")
  nfile <- attr(proto, "rcvr_file_cols")
  nms <- colnames(proto)
  layout <- delim_layout(nms)
  added <- nms[-seq_len(nfile)]

  # fread may skip preamble lines on its own (a short first line), so find
  # the first ballot line by matching the header read's first row
  if (nrow(proto) > 0) {
    head_lines <- readLines(path, n = skip + 50L, warn = FALSE)
    norm <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
    target <- norm(unlist(proto[1, seq_len(nfile)], use.names = FALSE))
    is_first <- function(line) {
      r <- tryCatch(
        data.table::fread(text = line, header = FALSE, sep = ",", colClasses = "character", fill = TRUE),
        error = function(e) NULL
      )
      !is.null(r) && ncol(r) >= nfile && identical(norm(unlist(r[1, seq_len(nfile), with = FALSE], use.names = FALSE)), target)
    }
    at <- which(nzchar(head_lines))
    at <- at[vapply(head_lines[at], is_first, logical(1), USE.NAMES = FALSE)]
    if (length(at) > 0) skip <- at[1] - 1L
  }

  # header_processor() drops repeated header blocks (San Diego CA 2020); find
  # them by the contest name in the second-to-last column, as it does
  header <- is_header(path)
  rotated <- FALSE
  if (header) {
    probe_j <- nfile - 1L
    bare <- stringr::str_remove(nms[probe_j], paste0(rcvr_DUP_SENTINEL, "\\d+$"))
    probe <- parse_contest_cols(bare)$contest
    top <- data.table::fread(path, skip = 1, nrows = 1, header = TRUE, colClasses = "character")
    has_vf <- function(x) any(stringr::str_detect(x, stringr::fixed("(Vote For=")), na.rm = TRUE)
    rotated <- !has_vf(colnames(top)) && has_vf(unlist(top[1]))
  }

  con <- file(path, open = "r")
  on.exit(close(con), add = TRUE)
  invisible(readLines(con, n = skip, warn = FALSE))
  chunk_rows <- max(1L,as.integer(chunk_cells %/% nfile))

  parts <- list()
  hits <- integer(0)
  offset <- 0L
  repeat {
    lines <- readLines(con, n = chunk_rows, warn = FALSE)
    if (length(lines) == 0) break
    lines <- lines[nzchar(lines)]
    if (length(lines) == 0) next
    dt <- data.table::fread(text = lines, header = FALSE, sep = ",",
                            colClasses = "character", fill = TRUE)
    if (ncol(dt) > nfile) {
      extra <- dt[, (nfile + 1):ncol(dt)]
      if (any(vapply(extra, function(x) any(!is.na(x) & x != ""), logical(1)))) {
        cli::cli_abort(
          "{.file {path}}: rows {offset + 1}-{offset + nrow(dt)} have more fields than the header ({nfile}).",
          class = "rcvr_bad_delim"
        )
      }
      dt <- dt[, seq_len(nfile), with = FALSE]
    }
    for (j in seq_len(nfile - ncol(dt))) data.table::set(dt, j = paste0("V", ncol(dt) + 1L), value = NA_character_)
    data.table::setnames(dt, nms[seq_len(nfile)])
    for (a in added) data.table::set(dt, j = a, value = rep(NA_character_, nrow(dt)))

    if (header) hits <- c(hits, which(dt[[probe_j]] == probe) + offset)
    parts[[length(parts) + 1L]] <- delim_cells(dt, layout, offset)
    offset <- offset + nrow(dt)
    rm(dt, lines)
  }

  if (length(parts) == 0) {
    cli::cli_abort("{.file {path}} has no ballot rows.", class = "rcvr_bad_pairs")
  }
  cells <- bind_cells(parts)
  rm(parts)
  if (length(hits) > 0) {
    drop <- unlist(lapply(hits, function(h) (h - 1L - rotated):(h + 2L - rotated)))
    cells <- drop_cell_rows(cells, drop)
  }
  pairs_from_cells(cells, layout, attr(proto, "rcvr_magnitude"), path)
}
