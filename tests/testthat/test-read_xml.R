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
