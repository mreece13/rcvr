test_that("has_special_reader reports registered counties only", {
  expect_true(has_special_reader("NEW JERSEY", "CUMBERLAND"))
  expect_true(has_special_reader("TEXAS", "DENTON"))
  expect_false(has_special_reader("COLORADO", "CLEAR CREEK"))
})

test_that("read_special_cvr aborts when no reader is registered", {
  # COLORADO|CLEAR CREEK stays unregistered on purpose (see the registry test
  # below); FLORIDA|LEE was used here originally but is now a real entry.
  expect_error(
    read_special_cvr(fixture_path("special-tx-denton", "cvr.csv"), "COLORADO", "CLEAR CREEK"),
    class = "rcvr_no_special_reader"
  )
})

test_that("the registry is keyed on (state, county), not an if/else chain", {
  expect_type(rcvr_SPECIAL_READERS, "list")
  expect_true(all(stringr::str_detect(names(rcvr_SPECIAL_READERS), "^[A-Z ]+\\|[A-Z ]+$")))
  expect_true(all(vapply(rcvr_SPECIAL_READERS, is.function, logical(1))))
})

test_that("the New Jersey Cumberland reader keeps only cast votes and squishes names", {
  pairs <- read_special_cvr(
    fixture_path("special-nj-cumberland", "cvr.csv"), "NEW JERSEY", "CUMBERLAND"
  )

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_equal(nrow(pairs), 4)  # 2 cast votes + 1 synthesised undervote per ballot
  expect_true("JOSEPH R BIDEN" %in% pairs$raw_candidate)
  expect_false(any(stringr::str_detect(pairs$raw_candidate, "^ | $")))
})

test_that("the New Jersey Cumberland reader synthesises undervotes", {
  pairs <- read_special_cvr(
    fixture_path("special-nj-cumberland", "cvr.csv"), "NEW JERSEY", "CUMBERLAND"
  )
  mayor2 <- dplyr::filter(pairs, contest == "MAYOR", cvr_id == 2L)
  expect_equal(mayor2$raw_candidate, "undervote")
})

test_that("the Texas Denton reader maps its own column names onto the contract", {
  pairs <- read_special_cvr(
    fixture_path("special-tx-denton", "cvr.csv"), "TEXAS", "DENTON"
  )

  expect_setequal(pairs$contest, c("US PRESIDENT", "MAYOR"))
  expect_equal(unique(pairs$precinct), "001")
  expect_true("DEM JOSEPH R BIDEN" %in% pairs$raw_candidate)
  expect_true(all(is.na(pairs$rank)))
})

test_that("the FLORIDA WALTON reader emits the pairs contract", {
  pairs <- read_special_cvr(
    fixture_path("special-fl-walton", "cvr.csv"), "FLORIDA", "WALTON"
  )

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_setequal(pairs$contest, c("US PRESIDENT", "MAYOR"))
  expect_true("undervote" %in% pairs$raw_candidate)
  expect_true(all(is.na(pairs$rank)))
})

test_that("the FLORIDA LEE reader emits the pairs contract", {
  pairs <- read_special_cvr(
    fixture_path("special-fl-lee"), "FLORIDA", "LEE"
  )

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_setequal(pairs$contest, c("US PRESIDENT", "MAYOR"))
  expect_true("undervote" %in% pairs$raw_candidate)
  expect_true(all(is.na(pairs$rank)))
})

test_that("the FLORIDA MANATEE reader emits the pairs contract", {
  pairs <- read_special_cvr(
    fixture_path("special-fl-manatee"), "FLORIDA", "MANATEE"
  )

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_setequal(pairs$contest, c("US PRESIDENT", "MAYOR"))
  expect_true("undervote" %in% pairs$raw_candidate)
  expect_true(all(is.na(pairs$rank)))
})

test_that("the FLORIDA MARION reader emits the pairs contract", {
  pairs <- read_special_cvr(
    fixture_path("special-fl-marion"), "FLORIDA", "MARION"
  )

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_setequal(pairs$contest, c("US PRESIDENT", "MAYOR"))
  expect_true("undervote" %in% pairs$raw_candidate)
  expect_true(all(is.na(pairs$rank)))
})

test_that("the FLORIDA SANTA ROSA reader emits the pairs contract", {
  pairs <- read_special_cvr(
    fixture_path("special-fl-santa-rosa"), "FLORIDA", "SANTA ROSA"
  )

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_setequal(pairs$contest, c("US PRESIDENT", "MAYOR"))
  expect_true("undervote" %in% pairs$raw_candidate)
  expect_true(all(is.na(pairs$rank)))
})

test_that("the FLORIDA SARASOTA reader emits the pairs contract", {
  pairs <- read_special_cvr(
    fixture_path("special-fl-sarasota"), "FLORIDA", "SARASOTA"
  )

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_setequal(pairs$contest, c("US PRESIDENT", "MAYOR"))
  expect_true("undervote" %in% pairs$raw_candidate)
  expect_true(all(is.na(pairs$rank)))
})

test_that("the PENNSYLVANIA ALLEGHENY reader emits the pairs contract", {
  pairs <- read_special_cvr(
    fixture_path("special-pa-allegheny"), "PENNSYLVANIA", "ALLEGHENY"
  )

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_setequal(pairs$contest, c("US PRESIDENT", "MAYOR"))
  expect_true("undervote" %in% pairs$raw_candidate)
  expect_true(all(is.na(pairs$rank)))
})

test_that("the TEXAS MONTGOMERY reader emits the pairs contract", {
  pairs <- read_special_cvr(
    fixture_path("special-tx-montgomery", "cvr.csv"), "TEXAS", "MONTGOMERY"
  )

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_setequal(pairs$contest, c("US PRESIDENT", "MAYOR"))
  expect_true("undervote" %in% pairs$raw_candidate)
  expect_true(all(is.na(pairs$rank)))
})

test_that("a special county seeds through the same gen_metadata as every other type", {
  meta <- read_special_cvr(
    fixture_path("special-tx-denton", "cvr.csv"), "TEXAS", "DENTON"
  ) |>
    gen_metadata(
      "SPECIAL", fixture_path("special-tx-denton", "cvr.csv"),
      "2020 General", "TEXAS", "DENTON"
    )

  expect_true(all(rcvr_SEED_COLS %in% colnames(meta)))
  # the party prefix is a seed, stripped from candidate and lifted into party
  biden <- dplyr::filter(meta, raw_candidate == "DEM JOSEPH R BIDEN")
  expect_equal(biden$candidate, "JOSEPH R BIDEN")
  expect_equal(biden$party_detailed, "DEMOCRAT")
})
