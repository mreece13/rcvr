test_that("read_xml_cvr returns a pairs frame, one cvr_id per file", {
  pairs <- read_xml_cvr(fixture_path("xml"))

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_setequal(pairs$cvr_id, c(1L, 2L))
  expect_setequal(pairs$contest, c("US PRESIDENT", "MAYOR"))
})

test_that("read_xml_cvr reads the precinct off PrecinctSplit", {
  pairs <- read_xml_cvr(fixture_path("xml"))
  expect_setequal(pairs$precinct, c("PRECINCT 001", "PRECINCT 002"))
})

test_that("a write-in option becomes WRITEIN, not the written text", {
  pairs <- read_xml_cvr(fixture_path("xml"))
  mayor1 <- dplyr::filter(pairs, contest == "MAYOR", cvr_id == 1L)
  expect_equal(mayor1$raw_candidate, "WRITEIN")
})

test_that("Undervotes = 1 becomes an undervote row", {
  pairs <- read_xml_cvr(fixture_path("xml"))
  mayor2 <- dplyr::filter(pairs, contest == "MAYOR", cvr_id == 2L)
  expect_equal(mayor2$raw_candidate, "undervote")
})

test_that("XML carries no party and no rank", {
  pairs <- read_xml_cvr(fixture_path("xml"))
  expect_true(all(is.na(pairs$raw_party)))
  expect_true(all(is.na(pairs$rank)))
  expect_type(pairs$rank, "integer")
})

test_that("gen_metadata seeds an XML county with the full store key", {
  meta <- read_xml_cvr(fixture_path("xml")) |>
    gen_metadata("XML", fixture_path("xml"), "2020 General", "TEXAS", "POLK")

  expect_true(all(rcvr_SEED_COLS %in% colnames(meta)))
  expect_true("JOSEPH R BIDEN" %in% meta$raw_candidate)
  expect_equal(unique(meta$state), "TEXAS")
})

test_that("a ballot with no PrecinctSplit parses with NA precinct, not a crash", {
  pairs <- read_xml_cvr(fixture_path("xml-no-precinct"))
  expect_true(is.na(pairs$precinct))
  expect_equal(pairs$raw_candidate, "JOSEPH R BIDEN")
})

test_that("only the marked Options sibling is kept, not the first one", {
  pairs <- read_xml_cvr(fixture_path("xml-multi-options"))
  expect_equal(pairs$raw_candidate, "BOB SMITH")
  expect_false("ALICE JONES" %in% pairs$raw_candidate)
})

test_that("a marked Options sibling with no Name and no WriteInData aborts instead of silently dropping the mark", {
  expect_error(
    read_xml_cvr(fixture_path("xml-unnamed-option")),
    class = "rcvr_unnamed_option"
  )
})

test_that("read_xml_cvr aborts with a classed condition on a directory with no XML files", {
  expect_error(
    read_xml_cvr(fixture_path("xml-empty")),
    class = "rcvr_no_files"
  )
})

test_that("a vote-for-2 contest with two marked options emits two rows", {
  pairs <- read_xml_cvr(fixture_path("xml-vote-for-2"))
  expect_equal(nrow(pairs), 2L)
  expect_true(all(pairs$cvr_id == 1L))
  expect_true(all(pairs$contest == "CITY COUNCIL"))
  expect_setequal(pairs$raw_candidate, c("ALICE JONES", "BOB SMITH"))
})

hart_sheet <- function(dir, name, batch, seq, sheet, contest, choice) {
  writeLines(sprintf(paste0(
    '<?xml version="1.0" encoding="utf-8"?><Cvr xmlns="http://tempuri.org/CVRDesign.xsd">',
    '<Contests><Contest><Name>%s</Name><Id>c-%s</Id><Options><Option><Name>%s</Name>',
    '<Id>o1</Id><Value>1</Value></Option></Options></Contest></Contests>',
    '<BatchSequence>%s</BatchSequence><SheetNumber>%s</SheetNumber>',
    '<PrecinctSplit><Name>P1</Name><Id>p1</Id></PrecinctSplit>',
    '<BatchNumber>%s</BatchNumber><CvrGuid>%s</CvrGuid></Cvr>'),
    contest, contest, choice, seq, sheet, batch, name), file.path(dir, paste0(name, ".xml")))
}

test_that("Hart sheets of one ballot are linked by scan order", {
  dir <- withr::local_tempdir()
  # file names sort in a different order from the scan, as Hart GUIDs do
  hart_sheet(dir, "d", 7, 10, 1, "PRESIDENT", "BIDEN")   # ballot A, sheet 1
  hart_sheet(dir, "a", 7, 11, 2, "MEASURE A", "YES")     # ballot A, sheet 2
  hart_sheet(dir, "c", 7, 12, 1, "PRESIDENT", "TRUMP")   # ballot B, one sheet
  hart_sheet(dir, "b", 8, 13, 2, "MEASURE A", "NO")      # other batch: not linked
  pairs <- read_xml_cvr(dir)
  # sorted files: a, b, c, d -> sheet cvr_ids 1..4
  a_ballot <- pairs$cvr_id[pairs$raw_candidate == "BIDEN"]
  expect_equal(pairs$cvr_id[pairs$raw_candidate == "YES"], a_ballot)
  expect_equal(a_ballot, 4L)
  expect_equal(dplyr::n_distinct(pairs$cvr_id), 3L)
  expect_false(pairs$cvr_id[pairs$raw_candidate == "TRUMP"] == a_ballot)
})

test_that("hart_ballot_ids() leaves a sheet with a missing position field as its own ballot", {
  s <- tibble::tibble(i = 1:4, batch = c("1", "1", NA, "1"), seq = c("1", "2", "3", "3"),
                      sheet = c("1", "2", "2", "1"))
  expect_equal(hart_ballot_ids(s), c(1L, 1L, 3L, 4L))
  s$batch[1] <- NA
  expect_equal(hart_ballot_ids(s), 1:4)
})

test_that("read_xml_cvr signals progress, ending at the last file", {
  n_files <- length(list.files(fixture_path("xml"), pattern = "\\.xml$", recursive = TRUE))
  withr::local_options(progressr.enable = TRUE)
  msgs <- character()
  withCallingHandlers(
    pairs <- read_xml_cvr(fixture_path("xml")),
    progression = function(cond) {
      if (isTRUE(nzchar(cond$message))) msgs <<- c(msgs, cond$message)
    }
  )
  expect_gt(length(msgs), 0)
  expect_equal(utils::tail(msgs, 1), sprintf("XML sheet %s of %s", n_files, n_files))
})
