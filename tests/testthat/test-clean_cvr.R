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

  err <- rlang::catch_cnd(
    clean_cvr(plain_path(), type = "delim", metadata = partial, verbose = FALSE)
  )
  expect_true("DONALD J TRUMP" %in% err$pairs$raw_candidate)
  expect_match(conditionMessage(err), "DONALD J TRUMP")
})

test_that("clean_cvr never silently returns fewer rows than the pairs frame", {
  pairs <- read_delim_cvr(plain_path()) |> pairs_from_delim()
  clean <- clean_cvr(plain_path(), type = "delim", metadata = full_store(), verbose = FALSE)

  expect_equal(nrow(clean), nrow(pairs))
})

test_that("clean_cvr tolerates a store slice that still carries its key columns", {
  # spec C hands rcvr a slice filtered on (election, state, county); those
  # columns must not collide with anything or duplicate rows
  clean <- clean_cvr(plain_path(), type = "delim", metadata = full_store(), verbose = FALSE)
  pairs <- read_delim_cvr(plain_path()) |> pairs_from_delim()

  expect_equal(nrow(clean), nrow(pairs))
  expect_false(any(stringr::str_detect(colnames(clean), "\\.x$|\\.y$")))
})
