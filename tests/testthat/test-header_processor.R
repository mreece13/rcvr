test_that("is_header detects the vendor election-title first cell", {
  expect_true(is_header(fixture_path("delim-header", "cvr.csv")))
  expect_false(is_header(fixture_path("delim-plain", "cvr.csv")))
})

test_that("header_processor concatenates the three header rows and drops them", {
  raw <- header_processor(fixture_path("delim-header", "cvr.csv"))

  expect_equal(nrow(raw), 2)
  expect_true(any(stringr::str_detect(colnames(raw), "^MAYOR\\|\\|JANE DOE\\|\\|DEM")))
})

test_that("the dedup suffix never reaches a key column", {
  pairs <- read_delim_cvr(fixture_path("delim-header", "cvr.csv")) |>
    pairs_from_delim()

  expect_setequal(pairs$contest, "MAYOR")
  expect_setequal(stats::na.omit(pairs$raw_party), c("DEM", "REP"))
  expect_false(any(stringr::str_detect(pairs$raw_party, "_\\d+$"), na.rm = TRUE))
  expect_false(any(stringr::str_detect(pairs$raw_candidate, "__RCVRDUP__")))
})
