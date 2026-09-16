test_that("a run after a named column inherits the name and sets magnitude", {
  res <- resolve_placeholder_cols(c("Precinct", "COMMISSIONER", "V3", "V4"))

  expect_equal(
    res$names,
    c(
      "Precinct", "COMMISSIONER",
      paste0("COMMISSIONER", rcvr_DUP_SENTINEL, 1:2)
    )
  )
  expect_equal(res$magnitude, c("COMMISSIONER" = 3L))
})

test_that("two runs each resolve against their own parent", {
  res <- resolve_placeholder_cols(c("A", "V2", "B", "V4", "V5"))

  expect_equal(
    res$names,
    c(
      "A", paste0("A", rcvr_DUP_SENTINEL, 1),
      "B", paste0("B", rcvr_DUP_SENTINEL, 1:2)
    )
  )
  expect_equal(res$magnitude, c("A" = 2L, "B" = 3L))
})

test_that("a run starting at column 1 is left alone and warns", {
  expect_warning(
    res <- resolve_placeholder_cols(c("V1", "V2", "MAYOR")),
    class = "rcvr_unresolved_placeholder_cols"
  )

  expect_equal(res$names, c("V1", "V2", "MAYOR"))
  expect_length(res$magnitude, 0)
})

test_that("a run never inherits a key or metadata column's name", {
  expect_warning(
    res <- resolve_placeholder_cols(c("CvrNumber", "V2", "MAYOR")),
    class = "rcvr_unresolved_placeholder_cols"
  )

  expect_equal(res$names, c("CvrNumber", "V2", "MAYOR"))
  expect_length(res$magnitude, 0)

  expect_warning(
    res <- resolve_placeholder_cols(c("precinct", "V2")),
    class = "rcvr_unresolved_placeholder_cols"
  )
  expect_equal(res$names, c("precinct", "V2"))
})

test_that("no placeholders means an unchanged vector and no warning", {
  nms <- c("Precinct", "MAYOR", "US PRESIDENT")

  expect_no_warning(res <- resolve_placeholder_cols(nms))
  expect_equal(res$names, nms)
  expect_length(res$magnitude, 0)
})

test_that("both the fread and readxl placeholder forms are resolved", {
  res <- resolve_placeholder_cols(c("A", "V2", "B", "...4"))

  expect_equal(
    res$names,
    c(
      "A", paste0("A", rcvr_DUP_SENTINEL, 1),
      "B", paste0("B", rcvr_DUP_SENTINEL, 1)
    )
  )
  expect_equal(res$magnitude, c("A" = 2L, "B" = 2L))
})

test_that("sentinel = FALSE assigns the bare parent name and no magnitude", {
  res <- resolve_placeholder_cols(c("A", "V2", "V3"), sentinel = FALSE)

  expect_equal(res$names, c("A", "A", "A"))
  expect_length(res$magnitude, 0)
})

test_that("a multi-seat delim CVR carries no placeholder contest and a real magnitude", {
  path <- fixture_path("delim-multiseat", "cvr.csv")

  pairs <- read_delim_cvr(path) |> pairs_from_delim(path)

  expect_false(any(stringr::str_detect(pairs$contest, rcvr_PLACEHOLDER_RE)))
  expect_setequal(pairs$contest, c("COUNTY COMMISSIONER", "MAYOR"))

  meta <- gen_metadata(
    pairs, "DELIM", path, "2020 General", "ARIZONA", "YUMA"
  )

  expect_false(any(stringr::str_detect(meta$contest, rcvr_PLACEHOLDER_RE)))
  expect_equal(
    unique(meta$magnitude[meta$contest == "COUNTY COMMISSIONER"]), 3L
  )
  expect_true(all(is.na(meta$magnitude[meta$contest == "MAYOR"])))

  # the whole point of resolving on the raw column names: the pairs frame and
  # the metadata agree, so neither join guard fires
  expect_no_error(join_metadata(pairs, meta, "ARIZONA", "YUMA"))
})
