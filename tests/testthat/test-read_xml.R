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

test_that("a vote-for-2 contest with two marked options emits two rows", {
  pairs <- read_xml_cvr(fixture_path("xml-vote-for-2"))
  expect_equal(nrow(pairs), 2L)
  expect_true(all(pairs$cvr_id == 1L))
  expect_true(all(pairs$contest == "CITY COUNCIL"))
  expect_setequal(pairs$raw_candidate, c("ALICE JONES", "BOB SMITH"))
})
