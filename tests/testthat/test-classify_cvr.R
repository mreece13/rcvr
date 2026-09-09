test_that("classify_cvr still aborts on an ambiguous directory when no type is declared", {
  expect_error(
    classify_cvr(fixture_path("delim-mixed"), verbose = FALSE),
    "Multiple file types"
  )
})

test_that("resolve_reader uses the declared type and ignores the stray file", {
  res <- resolve_reader(fixture_path("delim-mixed"), type = "delim")

  expect_equal(res$type, "DELIM")
  expect_true(stringr::str_detect(res$path, "cvr\\.csv$"))
})

test_that("resolve_reader picks DELIM-MULTI when the declared delim dir has many files", {
  res <- resolve_reader(fixture_path("delim-multi"), type = "delim")
  expect_equal(res$type, "DELIM-MULTI")
})

test_that("resolve_reader passes a single declared delim file straight through", {
  res <- resolve_reader(fixture_path("delim-plain", "cvr.csv"), type = "delim")

  expect_equal(res$type, "DELIM")
  expect_true(stringr::str_detect(res$path, "cvr\\.csv$"))
})

test_that("resolve_reader rejects an unknown declared type", {
  expect_error(
    resolve_reader(fixture_path("delim-plain", "cvr.csv"), type = "parquet"),
    class = "rcvr_bad_type"
  )
})

test_that("resolve_reader falls back to sniffing when type is NULL", {
  res <- resolve_reader(fixture_path("delim-plain", "cvr.csv"), type = NULL)
  expect_equal(res$type, "DELIM")
})

test_that("resolve_reader picks DELIM-MULTI for a declared delim dir with a stray file", {
  res <- resolve_reader(fixture_path("delim-multi-mixed"), type = "delim")
  expect_equal(res$type, "DELIM-MULTI")
})
