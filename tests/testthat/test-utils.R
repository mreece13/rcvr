test_that("assert_pairs accepts a well-formed pairs frame", {
  pairs <- tibble::tibble(
    cvr_id = 1L, precinct = "001", contest = "MAYOR",
    raw_candidate = "JANE DOE", raw_party = NA_character_, rank = NA_integer_
  )
  expect_silent(assert_pairs(pairs))
})

test_that("assert_pairs aborts on a zero-row pairs frame instead of passing it vacuously", {
  pairs <- tibble::tibble(
    cvr_id = integer(0), precinct = character(0), contest = character(0),
    raw_candidate = character(0), raw_party = character(0), rank = integer(0)
  )
  expect_error(assert_pairs(pairs), class = "rcvr_bad_pairs")
})

test_that("assert_pairs aborts when a contract column is missing", {
  pairs <- tibble::tibble(cvr_id = 1L, contest = "MAYOR")
  expect_error(assert_pairs(pairs), class = "rcvr_bad_pairs")
})

test_that("assert_pairs aborts when a contract column has the wrong type", {
  pairs <- tibble::tibble(
    cvr_id = 1L, precinct = "001", contest = "MAYOR",
    raw_candidate = "JANE DOE", raw_party = NA_character_, rank = "1"
  )
  expect_error(assert_pairs(pairs), class = "rcvr_bad_pairs")
})
