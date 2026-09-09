delim_pairs <- function() {
  read_delim_cvr(fixture_path("delim-plain", "cvr.csv")) |> pairs_from_delim()
}

test_that("gen_metadata emits the full store key from its arguments", {
  meta <- gen_metadata(
    delim_pairs(),
    type = "DELIM",
    path = fixture_path("delim-plain", "cvr.csv"),
    election = "2020 General",
    state = "COLORADO",
    county = "CLEAR CREEK"
  )

  expect_true(all(rcvr_SEED_COLS %in% colnames(meta)))
  expect_equal(unique(meta$election), "2020 General")
  expect_equal(unique(meta$state), "COLORADO")
  expect_equal(unique(meta$county), "CLEAR CREEK")
})

test_that("the store key is unique within a seeded county", {
  meta <- gen_metadata(
    delim_pairs(), "DELIM", fixture_path("delim-plain", "cvr.csv"),
    "2020 General", "COLORADO", "CLEAR CREEK"
  )

  key <- dplyr::select(meta, election, state, county, contest, raw_candidate)
  expect_equal(nrow(key), nrow(dplyr::distinct(key)))
})

test_that("gen_metadata seeds candidate and party_detailed but never a `party` column", {
  meta <- gen_metadata(
    delim_pairs(), "DELIM", fixture_path("delim-plain", "cvr.csv"),
    "2020 General", "COLORADO", "CLEAR CREEK"
  )

  expect_false("party" %in% colnames(meta))
  biden <- dplyr::filter(meta, raw_candidate == "JOSEPH R BIDEN")
  expect_equal(biden$party_detailed, "DEMOCRAT")
  expect_equal(biden$candidate, "JOSEPH R BIDEN")
})

test_that("undervote rows are not seeded into the store", {
  meta <- gen_metadata(
    delim_pairs(), "DELIM", fixture_path("delim-plain", "cvr.csv"),
    "2020 General", "COLORADO", "CLEAR CREEK"
  )
  expect_false(any(stringr::str_detect(meta$raw_candidate, "^undervote$")))
})

test_that("magnitude is NA when no 'Vote For' string is present, never a rank", {
  meta <- gen_metadata(
    delim_pairs(), "DELIM", fixture_path("delim-plain", "cvr.csv"),
    "2020 General", "COLORADO", "CLEAR CREEK"
  )
  expect_true(all(is.na(meta$magnitude)))
  expect_false("rank" %in% colnames(meta))
})

test_that("seed_party recognises abbreviations in the party field and in the name", {
  expect_equal(seed_party("DEM", NA_character_), "DEMOCRAT")
  expect_equal(seed_party("REP", NA_character_), "REPUBLICAN")
  expect_equal(seed_party(NA_character_, "JANE DOE (LBT)"), "LIBERTARIAN")
  expect_equal(seed_party(NA_character_, "WRITE-IN"), "WRITEIN")
  expect_true(is.na(seed_party(NA_character_, "undervote")))
})

test_that("seed_candidate strips party prefixes, parentheticals, and diacritics", {
  expect_equal(seed_candidate("DEM JANE DOE"), "JANE DOE")
  expect_equal(seed_candidate("JOHN ROE (REP)"), "JOHN ROE")
  expect_equal(seed_candidate("WRITE-IN"), "WI")
})

test_that("gen_metadata aborts with class rcvr_bad_metadata_args when election/state/county are invalid", {
  expect_error(
    gen_metadata(
      delim_pairs(), "DELIM", fixture_path("delim-plain", "cvr.csv"),
      NA_character_, "COLORADO", "CLEAR CREEK"
    ),
    class = "rcvr_bad_metadata_args"
  )
  expect_error(
    gen_metadata(
      delim_pairs(), "DELIM", fixture_path("delim-plain", "cvr.csv"),
      "2020 General", NA_character_, "CLEAR CREEK"
    ),
    class = "rcvr_bad_metadata_args"
  )
  expect_error(
    gen_metadata(
      delim_pairs(), "DELIM", fixture_path("delim-plain", "cvr.csv"),
      "2020 General", "COLORADO", NA_character_
    ),
    class = "rcvr_bad_metadata_args"
  )
})
