# Regression tests for the parser issues found while cleaning post-2020 CVRs
# (TODO.md, 2026-11-11). Each block names the county that exposed the bug.

repeated <- function() {
  path <- fixture_path("delim-repeated", "cvr.csv")
  read_delim_cvr(path) |> pairs_from_delim(path)
}

test_that("adjacent repeated contest names are one multi-seat contest (Outagamie WI, Hudson NJ)", {
  pairs <- repeated()
  expect_false(any(stringr::str_detect(pairs$contest, "\\.\\d+$|__RCVRDUP__")))
  vt <- dplyr::filter(pairs, contest == "Village Trustee")
  expect_setequal(vt$raw_candidate[vt$cvr_id == 1L], c("ANN", "BOB"))
  expect_equal(attr(pairs, "rcvr_magnitude")[["Village Trustee"]], 3L)
})

test_that("a non-adjacent repeat of a name is its own contest (Combined Locks WI)", {
  pairs <- repeated()
  later <- dplyr::filter(pairs, contest == "Village Trustee [2]")
  expect_equal(later$raw_candidate[later$cvr_id == 3L], "CARL")
  expect_false("Village Trustee [2]" %in% names(attr(pairs, "rcvr_magnitude")))
})

test_that("a trailing ': (id)' stays part of a contest-per-column name (Carson City NV)", {
  pairs <- repeated()
  q <- dplyr::filter(pairs, contest == "State Question No. 1")
  expect_equal(q$raw_candidate[order(q$cvr_id)][1:2], c("YES", "NO"))
})

test_that("[REDACTED] cells are redacted (Marion FL)", {
  pairs <- repeated()
  q <- dplyr::filter(pairs, contest == "State Question No. 1", cvr_id == 3L)
  expect_equal(q$raw_candidate, "redacted")
})

test_that("a ballot with no marks at all becomes undervotes, not nothing (Sauk WI, Georgia)", {
  pairs <- repeated()
  blank <- dplyr::filter(pairs, cvr_id == 4L)
  expect_gt(nrow(blank), 0)
  expect_true(all(blank$raw_candidate == "undervote"))
  expect_setequal(blank$contest, c("Mayor", "Village Trustee [2]", "State Question No. 1"))
})

test_that("repeated names read from Excel never carry a readxl ...NN suffix (Dane WI)", {
  res <- resolve_placeholder_cols(c("Precinct", "Alder 1", "Mayor", "Alder 1", "Alder 1"))
  expect_equal(
    res$names,
    c("Precinct", "Alder 1", "Mayor", "Alder 1 [2]", paste0("Alder 1 [2]", rcvr_DUP_SENTINEL, 1))
  )
  expect_equal(res$magnitude, c("Alder 1 [2]" = 2L))
})

test_that("a candidate-name second header row is folded into the names (Hawaii 2024)", {
  path <- fixture_path("delim-two-row-header", "cvr.csv")
  raw <- read_delim_cvr(path)
  expect_equal(nrow(raw), 3L)
  pairs <- pairs_from_delim(raw, path)
  pres <- dplyr::filter(pairs, contest == "President")
  expect_equal(pres$raw_candidate[order(pres$cvr_id)], c("(R) TRUMP", "(D) HARRIS", "(D) HARRIS"))
  # a colon inside a plain contest name is not a delimiter
  maui <- dplyr::filter(pairs, contest == "Maui: Salary Commission")
  expect_equal(maui$raw_candidate[order(maui$cvr_id)], c("Yes", "No"))
})

test_that("a ranked-choice '(n)' candidate suffix becomes the rank (Fort Collins CO)", {
  path <- fixture_path("delim-rcv", "cvr.csv")
  pairs <- read_delim_cvr(path) |> pairs_from_delim(path)
  one <- dplyr::filter(pairs, cvr_id == 1L)
  expect_equal(one$raw_candidate, c("Ann Lee", "Bo Kim"))
  expect_equal(one$rank, c(1L, 2L))
  expect_equal(dplyr::filter(pairs, cvr_id == 3L)$raw_candidate, "undervote")
  meta <- gen_metadata(pairs, "DELIM", path, "E", "S", "C")
  expect_equal(unique(meta$magnitude), 1L)
})

test_that("ClearCount's normalized Choice_ export reads as a wide delim file (Clark OH)", {
  path <- fixture_path("delim-clearcount-normalized", "cvr.csv")
  pairs <- read_delim_cvr(path) |> pairs_from_delim(path)
  expect_setequal(pairs$contest, c("Governor (Vote For=1)", "Local Liquor Option (Vote For=1)"))
  gov <- dplyr::filter(pairs, contest == "Governor (Vote For=1)")
  expect_equal(gov$raw_candidate, c("Mike DeWine and Jon Husted", "Nan Whaley", "undervote"))
  expect_equal(gov$raw_party, c("Republican", "Democratic", NA))
  expect_equal(unique(pairs$precinct), "36")
})

test_that("Hart's nested Contests/Contest/Options/Option XML is read (Tarrant TX)", {
  pairs <- read_xml_cvr(fixture_path("xml-hart-nested"))
  one <- dplyr::filter(pairs, cvr_id == 1L)
  expect_equal(one$contest, c("Mayor", "Council", "Board"))
  expect_equal(one$raw_candidate, c("ALICE JONES", "WRITEIN", "overvote"))
  expect_equal(unique(one$precinct), "0074501")
  expect_equal(dplyr::filter(pairs, cvr_id == 2L)$raw_candidate, "undervote")
})

test_that("a pairs frame whose every contest is NA aborts", {
  bad <- tibble::tibble(
    cvr_id = 1:2, precinct = NA_character_, contest = NA_character_,
    raw_candidate = "undervote", raw_party = NA_character_, rank = NA_integer_
  )
  expect_error(assert_pairs(bad), class = "rcvr_bad_pairs")
})

test_that("cards scanned back to back but sorted apart are linked by record id (Pueblo CO)", {
  raw <- tibble::tibble(
    TabulatorNum = "1", BatchId = "B1",
    RecordId = c("1", "3", "2", "4"),
    ballot_style = "A",
    `Mayor` = c("Smith", "Jones", "", ""),
    `President` = c("", "", "X", "Y")
  )
  expect_warning(out <- fix_fragmentation(raw, "p.csv"))
  expect_equal(nrow(out), 2L)
  expect_equal(out$Mayor, c("Smith", "Jones"))
  expect_equal(out$President, c("X", "Y"))
})

test_that("fix_fragmentation warns without a path instead of erroring", {
  raw <- tibble::tibble(
    BatchId = rep("B1", 6), ballot_style = rep("A", 6),
    `Mayor` = c("Smith", "Jones", "", "", "Lee", ""),
    `President` = c("", "", "X", "Y", "", "Z")
  )
  expect_warning(fix_fragmentation(raw), class = "rcvr_unlinked_cards")
})

test_that("a Dominion 4-row header does not warn about its unnamed metadata columns", {
  expect_no_warning(header_processor(fixture_path("delim-header", "cvr.csv")))
})

test_that("a nested zip is read through an outer.zip::inner.zip path (Hardin OH)", {
  src <- normalizePath(fixture_path("xml"))
  work <- tempfile(); dir.create(work)
  inner <- file.path(work, "inner.zip")
  withr::with_dir(src, utils::zip(inner, list.files("."), flags = "-r9Xq"))
  outer <- file.path(work, "outer.zip")
  withr::with_dir(work, utils::zip(outer, "inner.zip", flags = "-9Xq"))

  expect_error(unzip_cvr(outer), class = "rcvr_no_files")
  a <- clean_cvr(src, type = "xml", metadata_only = TRUE, verbose = FALSE,
                 election = "E", state = "S", county = "C")
  b <- clean_cvr(paste0(outer, "::inner.zip"), type = "xml", metadata_only = TRUE,
                 verbose = FALSE, election = "E", state = "S", county = "C")
  expect_equal(a, b)
})

test_that("read_cvr() output can be reused across clean_cvr() calls", {
  path <- fixture_path("delim-plain", "cvr.csv")
  pairs <- read_cvr(path, type = "delim")
  meta <- clean_cvr(pairs = pairs, metadata_only = TRUE, verbose = FALSE,
                    election = "E", state = "S", county = "C")
  expect_equal(
    meta,
    clean_cvr(path, type = "delim", metadata_only = TRUE, verbose = FALSE,
              election = "E", state = "S", county = "C")
  )
  expect_equal(
    clean_cvr(pairs = pairs, metadata = meta, verbose = FALSE),
    clean_cvr(path, type = "delim", metadata = meta, verbose = FALSE)
  )
})

test_that("the duckdb xlsx reader matches readxl (Dane WI memory)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("readxl")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  ok <- tryCatch({ DBI::dbExecute(con, "INSTALL excel; LOAD excel"); TRUE }, error = function(e) FALSE)
  skip_if_not(ok, "duckdb excel extension unavailable")

  # write each CSV fixture cell for cell (header row included) to an xlsx
  to_xlsx <- function(csv) {
    out <- tempfile(fileext = ".xlsx")
    DBI::dbExecute(con, sprintf(
      "COPY (SELECT * FROM read_csv(%s, header = false, all_varchar = true)) TO %s (FORMAT xlsx, HEADER false)",
      DBI::dbQuoteString(con, normalizePath(csv)), DBI::dbQuoteString(con, out)
    ))
    out
  }

  for (f in c(fixture_path("delim-repeated", "cvr.csv"),
              fixture_path("delim-multiseat", "cvr.csv"),
              fixture_path("delim-header", "cvr.csv"))) {
    x <- to_xlsx(f)
    a <- withr::with_options(list(rcvr.xlsx_reader = "readxl"), read_delim_cvr(x) |> pairs_from_delim(x))
    b <- withr::with_options(list(rcvr.xlsx_reader = "duckdb"), read_delim_cvr(x) |> pairs_from_delim(x))
    expect_equal(b, a, info = basename(dirname(f)))
    expect_equal(b, read_delim_cvr(f) |> pairs_from_delim(f), info = basename(dirname(f)))
  }
})

test_that("Hart contests sharing a Name but not an Id stay separate (Tarrant TX)", {
  dir <- withr::local_tempdir()
  ballot <- function(id, cand) sprintf(paste0(
    '<?xml version="1.0" encoding="utf-8"?><Cvr xmlns="http://tempuri.org/CVRDesign.xsd">',
    '<Contests><Contest><Name>Mayor</Name><Id>%s</Id><Options><Option><Name>%s</Name>',
    '<Id>o</Id><Value>1</Value></Option></Options></Contest>',
    '<Contest><Name>Prop A</Name><Id>p0000000-x</Id><Options><Option><Name>FOR</Name>',
    '<Id>o</Id><Value>1</Value></Option></Options></Contest></Contests>',
    '<PrecinctSplit><Name>1</Name><Id>i</Id></PrecinctSplit></Cvr>'), id, cand)
  writeLines(ballot("aaaaaaaa-1111", "JIM ROSS"), file.path(dir, "1.xml"))
  writeLines(ballot("bbbbbbbb-2222", "ARMIN MIZANI"), file.path(dir, "2.xml"))
  pairs <- read_xml_cvr(dir)
  expect_setequal(unique(pairs$contest), c("Mayor [aaaaaaaa]", "Mayor [bbbbbbbb]", "Prop A"))
  expect_equal(dplyr::filter(pairs, raw_candidate == "JIM ROSS")$contest, "Mayor [aaaaaaaa]")
  expect_false("contest_id" %in% names(pairs))
})

test_that("a CSV row with too few fields is padded and every row is read", {
  # fread() used to stop at such a row and return the rows before it
  # (Passaic NJ 2024: 3,531 of 204,153 ballots), which rcvr_truncated_read
  # caught. Read with fill = TRUE nothing stops early: the short row is
  # padded (ragged-right exports omit empty trailing cells, Pickaway OH 2020)
  # and the rows after it are kept.
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("CvrNumber,Precinct,MAYOR,COUNCIL", "1,P1,ANN,BOB", "2,P1,ANN", "3,P1,ANN,BOB"), f)
  pairs <- read_cvr(f, type = "delim")
  expect_equal(sort(unique(pairs$cvr_id)), 1:3)
  expect_equal(pairs$raw_candidate[pairs$cvr_id == 2L & pairs$contest == "COUNCIL"], "undervote")
})

test_that("a District column is the precinct, not a contest (Ocean NJ 2024)", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Cast Vote Record,Batch,District,MAYOR (101)",
               "1,B1,TOMS RIVER WARD 4 DISTRICT 45,ANN",
               "2,B1,TOMS RIVER WARD 4 DISTRICT 46,BOB"), f)
  pairs <- read_cvr(f)
  expect_setequal(unique(pairs$contest), "MAYOR (101)")
  expect_setequal(unique(pairs$precinct), c("TOMS RIVER WARD 4 DISTRICT 45", "TOMS RIVER WARD 4 DISTRICT 46"))
})

test_that("no ballot style and no precinct: no card linking, undervotes only for contests on the ballot (Salt Lake UT 2025)", {
  f <- withr::local_tempfile(fileext = ".csv")
  # Dominion candidate-per-column layout without BallotType or Precinct.
  # "0" = contest on the ballot, not voted; blank = not on the ballot.
  writeLines(c(
    "CvrNumber,TabulatorNum,BatchId,RecordId,MAYOR,MAYOR,COUNCIL,COUNCIL",
    ",,,,ANN,BOB,CAL,DEE",
    "1,1,1,1,1,0,,",
    "2,1,1,2,,,1,0",
    "3,1,1,3,,,0,0",
    "4,1,1,4,0,1,,"
  ), f)
  pairs <- read_cvr(f)
  expect_equal(dplyr::n_distinct(pairs$cvr_id), 4)
  under <- dplyr::filter(pairs, raw_candidate == "undervote")
  expect_equal(nrow(under), 1)
  expect_equal(under$contest, "COUNCIL")
})

test_that("a CSV-escaped quote in a name is read as one quote (Manatee FL)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Cast Vote Record,Precinct,State Senator,\"Fire \"\"District\"\" Seat\"",
    "1,101,\"DEM Anthony \"\"Tony\"\" Eldon\",Ann",
    "2,101,REP Ben Smith,Bob"
  ), path)
  pairs <- read_cvr(path, type = "delim")
  expect_true("DEM Anthony \"Tony\" Eldon" %in% pairs$raw_candidate)
  expect_true("Fire \"District\" Seat" %in% pairs$contest)
  expect_false(any(stringr::str_detect(c(pairs$raw_candidate, pairs$contest), stringr::fixed('""')), na.rm = TRUE))
  expect_false(is.null(attr(pairs, "rcvr_magnitude")))
})

test_that("a vendor's own undervotes scope each card to its filled cells (Manatee FL)", {
  # two cards per voter, no ballot style: card 1 holds President, card 2
  # holds Mayor. A blank means "not on this card", never an undervote.
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Cast Vote Record,Precinct,President,Mayor",
    "1,101,REP Ann,",
    "2,101,,Bob",
    "3,101,undervote,",
    "4,101,,undervote"
  ), path)
  pairs <- read_cvr(path, type = "delim")
  expect_equal(nrow(pairs), 4L)
  expect_equal(sum(pairs$raw_candidate == "undervote"), 2L)
  expect_setequal(pairs$contest[pairs$cvr_id == 1L], "President")
  expect_setequal(pairs$contest[pairs$cvr_id == 2L], "Mayor")
  expect_true(all(pairs$precinct == "101"))
})

test_that("blank cells still synthesise undervotes when the vendor writes none", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Cast Vote Record,Precinct,President,Mayor",
    "1,101,REP Ann,",
    "2,101,,Bob"
  ), path)
  pairs <- read_cvr(path, type = "delim")
  expect_equal(sum(pairs$raw_candidate == "undervote"), 2L)
})

test_that("edge whitespace is trimmed from contest and candidate names (Passaic NJ)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Cast Vote Record,Precinct,US Senate ,Mayor",
    "1,101,Tammy Duckworth ,  Ann Lee",
    "2,101,Bob,Carl Ray"
  ), path)
  pairs <- read_cvr(path, type = "delim")
  expect_true(all(c("Tammy Duckworth", "Ann Lee") %in% pairs$raw_candidate))
  expect_true("US Senate" %in% pairs$contest)
  expect_false(any(pairs$raw_candidate != stringr::str_trim(pairs$raw_candidate), na.rm = TRUE))
})

test_that("ragged-right rows are padded, not read as truncated (Pickaway OH)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Cast Vote Record,Precinct,For President,For Sheriff,Issue 1",
    "1,P1,Rep Ann,Rep Bob",
    "2,P1,Dem Cal,Dem Dee,Yes",
    "3,P2,Rep Ann"
  ), path)
  pairs <- read_cvr(path, type = "delim")
  expect_setequal(unique(pairs$contest), c("For President", "For Sheriff", "Issue 1"))
  expect_equal(length(unique(pairs$cvr_id)), 3L)
})

test_that("a header split by unquoted commas inside names is rebuilt (Fayette OH)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Cast Vote Record,Precinct,For Sheriff,Village Levy - Police, 2 mills, Continuing,For Coroner",
    "1,P1,Rep Ann,For the Tax Levy,Dem Cal",
    "2,P1,Dem Bob,Against the Tax Levy,Rep Dee"
  ), path)
  pairs <- read_cvr(path, type = "delim")
  expect_setequal(unique(pairs$contest),
                  c("For Sheriff", "Village Levy - Police, 2 mills, Continuing", "For Coroner"))
  expect_false(any(pairs$contest == "P1"))
})

test_that("rows wider than the header abort rather than becoming extra seats (Mono CA)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "CVRNumber,PrecinctId,President,Supervisor",
    "1,5,Ann,Bob",
    "2,5,Ann,Bob,Carl,Dee"
  ), path)
  expect_error(read_cvr(path, type = "delim"), class = "rcvr_bad_delim")
})

test_that("names that are not valid UTF-8 pass through the name clean-up unchanged (Bergen NJ)", {
  bad <- "Jos\xe9 \"\"Pepe\"\" Ruiz "
  pairs <- tibble::tibble(cvr_id = 1:2, precinct = "P1", contest = c("Mayor ", "Mayor"),
                          raw_candidate = c(bad, "Ann  "), raw_party = NA_character_, rank = NA_integer_)
  out <- trim_names(undouble_quotes(pairs))
  expect_identical(out$raw_candidate[1], bad)
  expect_equal(out$raw_candidate[2], "Ann")
  expect_equal(out$contest, c("Mayor", "Mayor"))
})

test_that("names that are not valid UTF-8 are read as Windows-1252 (Bergen NJ, Collin TX)", {
  pairs <- tibble::tibble(
    cvr_id = 1:4, precinct = "P1", contest = "Q1",
    raw_candidate = c("YES/S\xcd", "Timothy \x93Toby\x94 Wallis", "Ann", "B\x81b"),
    raw_party = NA_character_, rank = NA_integer_
  )
  out <- fix_encoding(pairs)
  expect_true(all(validUTF8(out$raw_candidate)))
  expect_equal(out$raw_candidate[1:3],
               c("YES/SÍ", "Timothy “Toby” Wallis", "Ann"))
  expect_equal(out$raw_candidate[4], "B\u0081b")

  path <- withr::local_tempfile(fileext = ".csv")
  con <- file(path, "wb")
  writeBin(c(charToRaw("CvrNumber,Precinct,Mayor\n1,P1,\"Jos"), as.raw(0xe9),
             charToRaw(" \"\"Joe\"\" Morera\"\n2,P1,Ann\n")), con)
  close(con)
  read <- read_cvr(path, type = "delim")
  expect_true("José \"Joe\" Morera" %in% read$raw_candidate)
})

test_that("a CSV header that is not valid UTF-8 is still read (Okaloosa FL)", {
  path <- withr::local_tempfile(fileext = ".csv")
  con <- file(path, "wb")
  writeBin(c(charToRaw("Cast Vote Record,Precinct,Se"), as.raw(0xf1), charToRaw("or Mayor,Council\n1,P1,Ann,Bob\n2,P1,Cal,Dee\n")), con)
  close(con)
  pairs <- read_cvr(path, type = "delim")
  expect_equal(length(unique(pairs$cvr_id)), 2L)
  expect_equal(length(unique(pairs$contest)), 2L)
})
