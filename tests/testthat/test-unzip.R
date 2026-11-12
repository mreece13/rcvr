test_that("a contest on only some ballots is not mistaken for fragmentation", {
  raw <- tibble::tibble(
    precinct = c("1", "1", "2", "2"),
    ballot_style = c("A", "A", "B", "B"),
    `Mayor` = c("Smith", "Jones", "", ""),
    `President` = c("X", "Y", "X", "Y")
  )
  expect_equal(nrow(fix_fragmentation(raw)), 4L)
})

test_that("disjoint contests across ballot styles (a primary) are separate ballots", {
  raw <- tibble::tibble(
    CvrNumber = c("1", "2", "3", "4"),
    precinct = c("1", "1", "1", "1"),
    ballot_style = c("DEM", "DEM", "REP", "REP"),
    `Pres DEM` = c("Biden", "0", "", ""),
    `Pres REP` = c("", "", "Trump", "Haley")
  )
  expect_equal(nrow(fix_fragmentation(raw, "p.csv")), 4L)
})

test_that("a voter skipping the anchor contest is not a continuation card", {
  raw <- tibble::tibble(
    ballot_style = rep("A", 4),
    `Mayor` = c("Smith", "", "Jones", ""),
    `Council` = c("Lee", "Kim", "Lee", "Kim"),
    `President` = c("X", "Y", "", "X")
  )
  expect_equal(nrow(fix_fragmentation(raw, "s.csv")), 4L)
})

test_that("two-card ballots merge each card 2 into the card 1 above it", {
  raw <- tibble::tibble(
    CvrNumber = c("1", "2", "3", "4", "5", "6"),
    ballot_style = c("A [1]", "A [2]", "A [1]", "A [2]", "A [1]", "A [2]"),
    `Mayor` = c("Smith", "", "Jones", "", "0", ""),
    `President` = c("", "X", "", "Y", "", "0")
  )
  expect_warning(out <- fix_fragmentation(raw, "frag.csv"))
  expect_equal(nrow(out), 3L)
  expect_equal(out$Mayor, c("Smith", "Jones", "0"))
  expect_equal(out$President, c("X", "Y", "0"))
  expect_equal(out$CvrNumber, c("1", "3", "5"))
})

test_that("cards out of order within a batch are not linked", {
  raw <- tibble::tibble(
    BatchId = rep("B1", 6),
    ballot_style = rep("A", 6),
    `Mayor` = c("Smith", "Jones", "", "", "Lee", ""),
    `President` = c("", "", "X", "Y", "", "Z")
  )
  expect_warning(out <- fix_fragmentation(raw, "ed.csv"), class = "rcvr_unlinked_cards")
  expect_equal(nrow(out), 6L)
})

zip_dir <- function(src, zipfile, folder = FALSE) {
  # zip the contents of `src` (or `src` itself as a top-level folder)
  withr::with_dir(if (folder) dirname(src) else src, {
    members <- if (folder) basename(src) else list.files(".", recursive = TRUE)
    utils::zip(zipfile, members, flags = "-r9Xq")
  })
  zipfile
}

test_that("a zipped JSON export cleans like the expanded directory", {
  skip_if_not_installed("dominionCVR")
  src <- test_path("fixtures", "json")
  z <- zip_dir(normalizePath(src), tempfile(fileext = ".zip"))
  a <- clean_cvr(src, type = "json", metadata_only = TRUE, verbose = FALSE,
                 election = "E", state = "S", county = "C")
  b <- clean_cvr(z, type = "json", metadata_only = TRUE, verbose = FALSE,
                 election = "E", state = "S", county = "C")
  expect_equal(a, b)
})

test_that("a zip holding one top-level folder resolves to that folder", {
  src <- normalizePath(test_path("fixtures", "xml"))
  z <- zip_dir(src, tempfile(fileext = ".zip"), folder = TRUE)
  ex <- unzip_cvr(z)
  on.exit(unlink(dirname(ex), recursive = TRUE))
  expect_equal(basename(ex), "xml")
  a <- clean_cvr(src, type = "xml", metadata_only = TRUE, verbose = FALSE,
                 election = "E", state = "S", county = "C")
  b <- clean_cvr(z, type = "xml", metadata_only = TRUE, verbose = FALSE,
                 election = "E", state = "S", county = "C")
  expect_equal(a, b)
})

test_that("non-CVR members are skipped and an empty zip aborts", {
  d <- tempfile(); dir.create(d)
  writeLines("x", file.path(d, "image.png"))
  z <- zip_dir(d, tempfile(fileext = ".zip"))
  expect_error(unzip_cvr(z), class = "rcvr_no_files")
})
