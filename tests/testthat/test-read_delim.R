test_that("read_delim_cvr returns one row per ballot with renamed precinct", {
  raw <- read_delim_cvr(fixture_path("delim-plain", "cvr.csv"))

  expect_equal(nrow(raw), 3)
  expect_true("precinct" %in% colnames(raw))
  expect_equal(raw$precinct, c("001", "001", "002"))
})

test_that("pairs_from_delim splits contest||candidate||party into the pairs frame", {
  pairs <- read_delim_cvr(fixture_path("delim-plain", "cvr.csv")) |>
    pairs_from_delim()

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_setequal(pairs$contest, c("US PRESIDENT", "MAYOR"))
  expect_true(all(is.na(pairs$rank)))

  biden <- dplyr::filter(pairs, raw_candidate == "JOSEPH R BIDEN")
  expect_equal(biden$raw_party, "DEM")
  expect_equal(biden$cvr_id, 1L)
})

test_that("pairs_from_delim marks a skipped contest as undervote", {
  pairs <- read_delim_cvr(fixture_path("delim-plain", "cvr.csv")) |>
    pairs_from_delim()

  mayor <- dplyr::filter(pairs, contest == "MAYOR", cvr_id == 2L)
  expect_equal(mayor$raw_candidate, "undervote")
})

test_that("read_delim_multi_cvr row-binds every file in the directory", {
  raw <- read_delim_multi_cvr(fixture_path("delim-multi"))
  expect_equal(nrow(raw), 4)
})

test_that("read_delim_multi_cvr ignores a stray non-delim file in the directory", {
  raw <- read_delim_multi_cvr(fixture_path("delim-multi-mixed"))
  expect_equal(nrow(raw), 4)
})
