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

test_that("read_delim_cvr aborts with a real error, not object 'd' not found, on a bad extension", {
  err <- rlang::catch_cnd(
    read_delim_cvr(fixture_path("delim-bad-ext", "cvr.txt")),
    classes = "error"
  )
  expect_match(conditionMessage(err), "not a CSV or Excel file")
})

test_that("read_delim_multi_cvr row-binds every file in the directory", {
  raw <- read_delim_multi_cvr(fixture_path("delim-multi"))
  expect_equal(nrow(raw), 4)
})

test_that("read_delim_multi_cvr ignores a stray non-delim file in the directory", {
  raw <- read_delim_multi_cvr(fixture_path("delim-multi-mixed"))
  expect_equal(nrow(raw), 4)
})

test_that("read_delim_multi_cvr aborts with a classed condition on a directory with no delim files", {
  expect_error(
    read_delim_multi_cvr(fixture_path("delim-multi-empty")),
    class = "rcvr_no_files"
  )
})

test_that("a tab-separated .csv is split on tabs (Walton FL)", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "CVRNumber\tPrecinct\tPresident (100000)\tRep in Congress, Dist 1 (140010)",
    "1\t520-Santa Rosa Beach\tREP Donald J. Trump (74773)\tREP Matt Gaetz (74334)",
    "2\t510-Miramar Beach\tDEM Joseph R. Biden (74772)\tundervote"
  ), f)
  pairs <- read_delim_cvr(f) |> pairs_from_delim()
  expect_setequal(unique(pairs$contest), c("President (100000)", "Rep in Congress, Dist 1 (140010)"))
  expect_setequal(unique(pairs$precinct), c("520-Santa Rosa Beach", "510-Miramar Beach"))
})

test_that("an unquoted comma and space inside a name stays in the name (Ross OH)", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Cast Vote Record,Precinct,For Sheriff,For Levy,",
    "1,8 Chillicothe 2-D,Rep George W. Lavender, Jr. (71GLAVENDER71),For the Tax Levy,",
    "2,24 Buckskin,undervote,Against the Tax Levy,"
  ), f)
  pairs <- read_delim_cvr(f) |> pairs_from_delim()
  expect_setequal(unique(pairs$contest), c("For Sheriff", "For Levy"))
  expect_true("Rep George W. Lavender, Jr. (71GLAVENDER71)" %in% pairs$raw_candidate)
})

test_that("rows wider than the header with no comma and space to blame still abort", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("A,B,C", "1,x,y,z", "2,x,y,z"), f)
  expect_error(read_delim_cvr(f), class = "rcvr_bad_delim")
})
