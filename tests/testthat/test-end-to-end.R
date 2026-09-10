seed_and_clean <- function(path, type, state, county) {
  store <- clean_cvr(
    path, type = type, metadata_only = TRUE, verbose = FALSE,
    election = "2020 General", state = state, county = county
  )

  clean <- clean_cvr(
    path, type = type, metadata = store, verbose = FALSE,
    election = "2020 General", state = state, county = county
  )

  list(store = store, clean = clean)
}

test_that("a delim county seeds and cleans end to end", {
  res <- seed_and_clean(
    fixture_path("delim-plain", "cvr.csv"), "delim", "COLORADO", "CLEAR CREEK"
  )

  expect_true(all(rcvr_SEED_COLS %in% colnames(res$store)))
  expect_gt(nrow(res$clean), 0)
  expect_true(all(is.na(res$clean$rank)))
})

test_that("a delim-multi county seeds and cleans end to end", {
  res <- seed_and_clean(
    fixture_path("delim-multi"), "delim", "COLORADO", "DENVER"
  )
  expect_gt(nrow(res$clean), 0)
})

test_that("a header-style delim county seeds and cleans end to end", {
  res <- seed_and_clean(
    fixture_path("delim-header", "cvr.csv"), "delim", "CALIFORNIA", "SAN DIEGO"
  )
  expect_false(any(stringr::str_detect(res$store$raw_party, "_\\d+$"), na.rm = TRUE))
  expect_gt(nrow(res$clean), 0)
})

test_that("a json county seeds and cleans end to end, carrying rank", {
  testthat::skip_if_not_installed("dominionCVR")

  res <- seed_and_clean(fixture_path("json"), "json", "GEORGIA", "FRANKLIN")

  expect_gt(nrow(res$clean), 0)
  expect_true(any(!is.na(res$clean$rank)))
})

test_that("an xml county seeds and cleans end to end", {
  res <- seed_and_clean(fixture_path("xml"), "xml", "TEXAS", "POLK")

  expect_gt(nrow(res$clean), 0)
  expect_true(all(is.na(res$clean$rank)))
})

test_that("a special county seeds and cleans end to end", {
  res <- seed_and_clean(
    fixture_path("special-tx-denton", "cvr.csv"), "special", "TEXAS", "DENTON"
  )
  expect_gt(nrow(res$clean), 0)
})

test_that("every cleaned output carries rank, whatever the type", {
  paths <- list(
    list(fixture_path("delim-plain", "cvr.csv"), "delim", "COLORADO", "CLEAR CREEK"),
    list(fixture_path("xml"), "xml", "TEXAS", "POLK"),
    list(fixture_path("special-tx-denton", "cvr.csv"), "special", "TEXAS", "DENTON")
  )

  for (p in paths) {
    clean <- seed_and_clean(p[[1]], p[[2]], p[[3]], p[[4]])$clean
    expect_true("rank" %in% colnames(clean), info = p[[2]])
    expect_type(clean$rank, "integer")
  }
})

test_that("clean_cvr writes a parquet file when write_path is given", {
  skip_if_not_installed("arrow")

  out <- withr::local_tempfile(fileext = ".parquet")
  store <- clean_cvr(
    fixture_path("delim-plain", "cvr.csv"), type = "delim", metadata_only = TRUE,
    verbose = FALSE, election = "2020 General", state = "COLORADO", county = "CLEAR CREEK"
  )

  res <- clean_cvr(
    fixture_path("delim-plain", "cvr.csv"), type = "delim", metadata = store,
    verbose = FALSE, write_path = out
  )

  expect_equal(res, out)
  expect_true(fs::file_exists(out))
  expect_gt(nrow(arrow::read_parquet(out)), 0)
})
