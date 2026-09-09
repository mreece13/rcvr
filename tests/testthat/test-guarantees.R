plain_path <- function() fixture_path("delim-plain", "cvr.csv")

test_that("a store row wins over any regex change in gen_metadata (seeds, not rules)", {
  store <- read_delim_cvr(plain_path()) |>
    pairs_from_delim() |>
    gen_metadata("DELIM", plain_path(), "2020 General", "COLORADO", "CLEAR CREEK")

  # a human has hand-corrected the candidate name, punctuation and all
  store <- dplyr::mutate(
    store,
    candidate = dplyr::if_else(
      raw_candidate == "JOSEPH R BIDEN", "Joseph R. Biden Jr.", candidate
    )
  )

  before <- clean_cvr(plain_path(), type = "delim", metadata = store, verbose = FALSE)

  # simulate a regex change by stubbing the seeding helper to something absurd
  testthat::local_mocked_bindings(
    seed_candidate = function(candidate) stringr::str_to_lower(candidate)
  )

  after <- clean_cvr(plain_path(), type = "delim", metadata = store, verbose = FALSE)

  expect_equal(before$candidate, after$candidate)
  expect_true("Joseph R. Biden Jr." %in% after$candidate)
})

test_that("gen_metadata is not called at all when metadata is supplied", {
  store <- read_delim_cvr(plain_path()) |>
    pairs_from_delim() |>
    gen_metadata("DELIM", plain_path(), "2020 General", "COLORADO", "CLEAR CREEK")

  testthat::local_mocked_bindings(
    gen_metadata = function(...) stop("gen_metadata must not run in production")
  )

  expect_no_error(
    clean_cvr(plain_path(), type = "delim", metadata = store, verbose = FALSE)
  )
})

test_that("generate_metadata is ignored, with a message, when metadata is supplied", {
  store <- read_delim_cvr(plain_path()) |>
    pairs_from_delim() |>
    gen_metadata("DELIM", plain_path(), "2020 General", "COLORADO", "CLEAR CREEK")

  expect_message(
    clean_cvr(
      plain_path(), type = "delim", metadata = store,
      generate_metadata = TRUE, verbose = FALSE
    ),
    "generate_metadata"
  )
})

test_that("a missing store row aborts rather than shrinking the output", {
  store <- read_delim_cvr(plain_path()) |>
    pairs_from_delim() |>
    gen_metadata("DELIM", plain_path(), "2020 General", "COLORADO", "CLEAR CREEK")

  partial <- dplyr::filter(store, raw_candidate != "JANE DOE")

  expect_error(
    clean_cvr(plain_path(), type = "delim", metadata = partial, verbose = FALSE),
    class = "rcvr_unmatched_pairs"
  )
})
