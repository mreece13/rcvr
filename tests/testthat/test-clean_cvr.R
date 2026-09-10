plain_path <- function() fixture_path("delim-plain", "cvr.csv")

full_store <- function() {
  read_delim_cvr(plain_path()) |>
    pairs_from_delim() |>
    gen_metadata("DELIM", plain_path(), "2020 General", "COLORADO", "CLEAR CREEK")
}

test_that("clean_cvr returns one row per ballot-contest with store values attached", {
  clean <- clean_cvr(
    plain_path(),
    type = "delim",
    metadata = full_store(),
    verbose = FALSE
  )

  expect_true(all(c("office", "district", "candidate", "party_detailed", "magnitude") %in% colnames(clean)))
  biden <- dplyr::filter(clean, raw_candidate == "JOSEPH R BIDEN")
  expect_equal(biden$party_detailed, "DEMOCRAT")
})

test_that("clean_cvr keeps undervote rows, which have no store row", {
  clean <- clean_cvr(plain_path(), type = "delim", metadata = full_store(), verbose = FALSE)
  expect_true("undervote" %in% clean$raw_candidate)
})

test_that("clean_cvr aborts and names the pair when the store is missing a row", {
  partial <- dplyr::filter(full_store(), raw_candidate != "DONALD J TRUMP")

  expect_error(
    clean_cvr(plain_path(), type = "delim", metadata = partial, verbose = FALSE),
    class = "rcvr_unmatched_pairs"
  )

  # scoped to "error": dplyr (>= 1.2.0) signals a bare, payload-free
  # "dplyr_regroup" condition on every group_by() call (its own internal
  # test-instrumentation hook), which is not an error and must not be what
  # this assertion inspects
  err <- rlang::catch_cnd(
    clean_cvr(plain_path(), type = "delim", metadata = partial, verbose = FALSE),
    classes = "error"
  )
  expect_s3_class(err, "rcvr_unmatched_pairs")
  expect_true("DONALD J TRUMP" %in% err$pairs$raw_candidate)
  expect_match(conditionMessage(err), "DONALD J TRUMP")
})

test_that("clean_cvr never silently returns fewer rows than the pairs frame", {
  pairs <- read_delim_cvr(plain_path()) |> pairs_from_delim()
  clean <- clean_cvr(plain_path(), type = "delim", metadata = full_store(), verbose = FALSE)

  expect_equal(nrow(clean), nrow(pairs))
})

test_that("join_metadata aborts on an NA raw_candidate instead of silently keeping it", {
  # str_detect(NA, ...) is NA, and dplyr::filter() drops NA conditions; an
  # NA-keyed pair must never be treated as exempt the way "undervote" is
  pairs <- tibble::tibble(
    cvr_id = 1L,
    precinct = "001",
    contest = "MAYOR",
    raw_candidate = NA_character_,
    raw_party = NA_character_,
    rank = NA_integer_
  )
  store <- tibble::tibble(
    contest = "MAYOR",
    raw_candidate = "JANE DOE",
    raw_party = NA_character_,
    office = "MAYOR",
    district = NA_character_,
    candidate = "JANE DOE",
    party_detailed = NA_character_,
    magnitude = 1L
  )

  err <- rlang::catch_cnd(
    join_metadata(pairs, store, state = "X", county = "Y"),
    classes = "error"
  )
  expect_s3_class(err, "rcvr_unmatched_pairs")
  expect_true(anyNA(err$pairs$raw_candidate))
})

test_that("clean_cvr aborts with a classed condition when the type cannot be resolved", {
  expect_error(
    clean_cvr(fixture_path("dir-unrecognized"), verbose = FALSE),
    class = "rcvr_bad_type"
  )
})

test_that("join_metadata aborts on a store that fans out a (contest, raw_candidate) pair", {
  pairs <- tibble::tibble(
    cvr_id = 1L,
    precinct = "001",
    contest = "MAYOR",
    raw_candidate = "JANE DOE",
    raw_party = NA_character_,
    rank = NA_integer_
  )
  # two store rows spelling the same candidate's party two ways -- a fan-out
  # the left join in join_metadata() would silently multiply rows for
  store <- tibble::tibble(
    contest = c("MAYOR", "MAYOR"),
    raw_candidate = c("JANE DOE", "JANE DOE"),
    raw_party = c("DEM", "DEMOCRAT"),
    office = c("MAYOR", "MAYOR"),
    district = NA_character_,
    candidate = c("JANE DOE", "JANE DOE"),
    party_detailed = c("DEMOCRAT", "DEMOCRAT"),
    magnitude = 1L
  )

  err <- rlang::catch_cnd(
    join_metadata(pairs, store, state = "X", county = "Y"),
    classes = "error"
  )
  expect_s3_class(err, "rcvr_duplicate_metadata")
  expect_true("JANE DOE" %in% err$pairs$raw_candidate)
})

test_that("clean_cvr tolerates a store slice that still carries its key columns", {
  # spec C hands rcvr a slice filtered on (election, state, county); those
  # columns must not collide with anything or duplicate rows
  clean <- clean_cvr(plain_path(), type = "delim", metadata = full_store(), verbose = FALSE)
  pairs <- read_delim_cvr(plain_path()) |> pairs_from_delim()

  expect_equal(nrow(clean), nrow(pairs))
  expect_false(any(stringr::str_detect(colnames(clean), "\\.x$|\\.y$")))
})
