skip_if_no_dominion <- function() {
  testthat::skip_if_not_installed("dominionCVR")
}

test_that("read_dominion_manifests resolves ids to strings", {
  m <- read_dominion_manifests(fixture_path("json"))

  expect_equal(
    dplyr::filter(m$candidates, id == 100)$raw_candidate, "JOSEPH R BIDEN"
  )
  expect_equal(dplyr::filter(m$contests, id == 11)$contest, "COUNTY COMMISSIONER")
  expect_equal(dplyr::filter(m$contests, id == 11)$district, "5")
  expect_equal(dplyr::filter(m$contests, id == 11)$magnitude, 2L)
  expect_equal(dplyr::filter(m$parties, id == 1)$raw_party, "Democratic Party")
  expect_equal(dplyr::filter(m$precincts, id == 50)$precinct, "PRECINCT 001")
})

test_that("read_json_cvr returns a pairs frame with real strings, not ids", {
  skip_if_no_dominion()
  pairs <- read_json_cvr(fixture_path("json"))

  expect_named(
    pairs,
    c("cvr_id", "precinct", "contest", "raw_candidate", "raw_party", "rank")
  )
  expect_setequal(pairs$contest, c("US PRESIDENT", "COUNTY COMMISSIONER"))
  expect_true("JOSEPH R BIDEN" %in% pairs$raw_candidate)
  expect_equal(unique(pairs$precinct), "PRECINCT 001")
})

test_that("an empty Marks array becomes an undervote with NA rank", {
  skip_if_no_dominion()
  pairs <- read_json_cvr(fixture_path("json"))

  uv <- dplyr::filter(pairs, raw_candidate == "undervote")
  expect_equal(nrow(uv), 1)
  expect_equal(uv$contest, "COUNTY COMMISSIONER")
  expect_true(is.na(uv$rank))
  expect_true(is.na(uv$raw_party))
})

test_that("rank is populated for JSON and is not magnitude", {
  skip_if_no_dominion()
  pairs <- read_json_cvr(fixture_path("json"))

  voted <- dplyr::filter(pairs, raw_candidate != "undervote")
  expect_true(all(voted$rank == 1L))
  expect_type(pairs$rank, "integer")
})

test_that("read_json_cvr aborts with a classed condition on a directory with no CVR export files", {
  skip_if_no_dominion()
  expect_error(
    read_json_cvr(fixture_path("json-empty")),
    class = "rcvr_no_files"
  )
})

test_that("read_dominion_manifests aborts with a classed condition when a manifest file is missing", {
  expect_error(
    read_manifest_list(fixture_path("json-empty"), "DistrictManifest.json"),
    class = "rcvr_missing_manifest"
  )
})

test_that("read_json_cvr aborts when a manifest id fails to resolve", {
  skip_if_no_dominion()

  err <- rlang::catch_cnd(read_json_cvr(fixture_path("json-badids")), classes = "rcvr_unresolved_ids")
  expect_s3_class(err, "rcvr_unresolved_ids")
  expect_true(999 %in% err$ids)
})

test_that("gen_metadata seeds a JSON county with the full store key", {
  skip_if_no_dominion()
  meta <- read_json_cvr(fixture_path("json")) |>
    gen_metadata("JSON", fixture_path("json"), "2020 General", "GEORGIA", "FRANKLIN")

  expect_true(all(rcvr_SEED_COLS %in% colnames(meta)))
  expect_true("raw_candidate" %in% colnames(meta))
  expect_equal(unique(meta$county), "FRANKLIN")

  cc <- dplyr::filter(meta, contest == "COUNTY COMMISSIONER")
  expect_equal(unique(cc$district), "5")
  expect_equal(unique(cc$magnitude), 2L)

  biden <- dplyr::filter(meta, raw_candidate == "JOSEPH R BIDEN")
  expect_equal(biden$party_detailed, "DEMOCRAT")
})
