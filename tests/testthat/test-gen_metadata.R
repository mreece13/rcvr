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

test_that("gen_patterns escapes a metacharacter-bearing abbreviation for TRE, not just PCRE", {
  # in TRE (gsub()'s default engine), a backslash inside a bracket
  # expression is literal, so "([\\[\\]...])" terminates the class at the
  # first "]" and never actually escapes anything; gen_patterns() must
  # produce a pattern where "C.O" (a literal abbreviation containing a
  # metacharacter) matches only the literal string, not "C" + any-char + "O"
  pattern <- gen_patterns("C.O")
  expect_true(stringr::str_detect("C.O", stringr::regex(pattern, TRUE)))
  expect_false(stringr::str_detect("CXO", stringr::regex(pattern, TRUE)))
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

nonpartisan_meta <- function() {
  read_delim_cvr(fixture_path("delim-nonpartisan", "cvr.csv")) |>
    pairs_from_delim() |>
    gen_metadata(
      "DELIM", fixture_path("delim-nonpartisan", "cvr.csv"),
      "2020 General", "COLORADO", "CLEAR CREEK"
    )
}

test_that("a nonpartisan row carries both party_detailed and the nonpartisan flag", {
  meta <- nonpartisan_meta()
  board <- dplyr::filter(meta, contest == "SCHOOL BOARD")

  expect_true(all(board$party_detailed == "NONPARTISAN"))
  expect_true(all(board$nonpartisan))
})

test_that("a partisan row is not flagged nonpartisan", {
  meta <- nonpartisan_meta()
  pres <- dplyr::filter(meta, contest == "US PRESIDENT")

  expect_equal(pres$party_detailed, "DEMOCRAT")
  expect_false(any(pres$nonpartisan))
})

test_that("nonpartisan is logical and NA only when no party could be determined", {
  meta <- nonpartisan_meta()
  expect_type(meta$nonpartisan, "logical")
  expect_equal(is.na(meta$nonpartisan), is.na(meta$party_detailed))
})

test_that("nonpartisan is NA when party_detailed cannot be determined at all", {
  meta <- nonpartisan_meta()
  no_party <- dplyr::filter(meta, is.na(party_detailed))

  expect_true(nrow(no_party) > 0)
  expect_true(all(is.na(no_party$nonpartisan)))
  expect_type(meta$nonpartisan, "logical")
})

test_that("seed_party maps the nonpartisan abbreviations", {
  expect_equal(seed_party("NON", NA_character_), "NONPARTISAN")
  expect_equal(seed_party("NPN", NA_character_), "NONPARTISAN")
  expect_equal(seed_party("Nonpartisan", NA_character_), "NONPARTISAN")
})

test_that("seed_party matches a full manifest party name exactly", {
  expect_equal(seed_party("Democratic Party", NA_character_), "DEMOCRAT")
})

test_that("seed_party does not mis-seed a similar-but-different full party name", {
  out <- seed_party("Democratic-Republican Party", NA_character_)
  expect_false(identical(out, "DEMOCRAT"))
  expect_equal(out, "Democratic-Republican Party")
})

test_that("seed_party matches a nonpartisan full manifest party name exactly", {
  expect_equal(seed_party("Nonpartisan Party", NA_character_), "NONPARTISAN")
})
