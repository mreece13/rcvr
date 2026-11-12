# Large CSVs are streamed in row chunks (read_delim_chunked()); the result
# must be the same pairs frame as reading the file whole.

whole <- function(path) suppressWarnings(read_delim_cvr(path) |> pairs_from_delim(path))
chunked <- function(path, cells = 1) suppressWarnings(read_delim_chunked(path, chunk_cells = cells))

expect_same_pairs <- function(path) {
  expect_equal(
    as.data.frame(chunked(path)),
    as.data.frame(whole(path)),
    ignore_attr = TRUE
  )
}

test_that("chunked reading matches whole-file reading on the delim fixtures", {
  for (fx in c("delim-plain", "delim-header", "delim-two-row-header", "delim-multiseat",
               "delim-repeated", "delim-rcv", "delim-clearcount-normalized", "delim-redacted")) {
    expect_same_pairs(fixture_path(fx, "cvr.csv"))
  }
})

write_csv_lines <- function(lines) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  writeLines(lines, path)
  path
}

test_that("a repeated 4-row header block is dropped in chunked reading (San Diego CA)", {
  path <- write_csv_lines(c(
    "2020 General Election,5.1,,",
    "CvrNumber,Precinct,MAYOR,MAYOR",
    ",,JANE DOE,JOHN ROE",
    ",,DEM,REP",
    "1,001,1,0",
    "2020 General Election,5.1,,",
    "CvrNumber,Precinct,MAYOR,MAYOR",
    ",,JANE DOE,JOHN ROE",
    ",,DEM,REP",
    "2,001,0,1"
  ))
  pairs <- chunked(path)
  expect_equal(pairs$raw_candidate, c("JANE DOE", "JOHN ROE"))
  expect_same_pairs(path)
})

test_that("a Dominion header with the party row first is rebuilt (Pueblo CO 2022)", {
  path <- write_csv_lines(c(
    "CvrNumber,TabulatorNum,BatchId,RecordId,PrecinctPortion,BallotType,DEM,REP",
    "2022 Pueblo County General Election,5.13,,,,,,",
    ",,,,,,United States Senator (Vote For=1),United States Senator (Vote For=1)",
    ",,,,,,Michael Bennet,Joe O'Dea",
    "1,104,1,1,P1,9,1,0",
    "2,104,1,2,P1,9,0,1"
  ))
  pairs <- whole(path)
  expect_equal(unique(pairs$contest), "United States Senator (Vote For=1)")
  expect_equal(pairs$raw_candidate, c("Michael Bennet", "Joe O'Dea"))
  expect_equal(pairs$raw_party, c("DEM", "REP"))
  expect_same_pairs(path)
})

test_that("a Dominion header with names before contests is rotated (Pueblo CO 2022 recount)", {
  path <- write_csv_lines(c(
    "2022 General Election Recount,5.13,,,,,,",
    "CvrNumber,TabulatorNum,BatchId,RecordId,PrecinctPortion,BallotType,DEM,REP",
    ",,,,,,United States Senator (Vote For=1),United States Senator (Vote For=1)",
    ",,,,,,Michael Bennet,Joe O'Dea",
    "1,104,1,1,P1,9,1,0",
    "2,104,1,2,P1,9,0,1"
  ))
  pairs <- whole(path)
  expect_equal(unique(pairs$contest), "United States Senator (Vote For=1)")
  expect_equal(pairs$raw_candidate, c("Michael Bennet", "Joe O'Dea"))
  expect_equal(pairs$raw_party, c("DEM", "REP"))
  expect_equal(pairs$precinct, c("P1", "P1"))
  expect_same_pairs(path)
})

test_that("a column already named precinct is kept, not blanked", {
  path <- write_csv_lines(c("CvrNumber,precinct,Mayor", "1,001,SMITH", "2,002,JONES"))
  expect_equal(whole(path)$precinct, c("001", "002"))
})

test_that("cards out of file order link by Excel-quoted record ids (Riverside CA)", {
  raw <- tibble::tibble(
    TabulatorNum = '="2"',
    BatchId = '="1"',
    RecordId = c('="2"', '="1"', '="4"', '="3"'),
    precinct = "P1",
    ballot_style = "A",
    Mayor = c("", "SMITH", "", "JONES"),
    President = c("BIDEN", "", "TRUMP", "")
  )
  pairs <- suppressWarnings(pairs_from_delim(raw))
  expect_equal(length(unique(pairs$cvr_id)), 2L)
  expect_setequal(pairs$raw_candidate[pairs$cvr_id == 1L], c("SMITH", "BIDEN"))
})

test_that("one card with every contest does not block linking (Pueblo CO 2022)", {
  n <- 60
  raw <- tibble::tibble(
    CvrNumber = as.character(seq_len(2 * n + 2)),
    precinct = "P1",
    ballot_style = "A",
    Mayor = c(rep(c("SMITH", ""), n), "SMITH", "SMITH"),
    President = c(rep(c("", "X"), n), "X", "X")
  )
  pairs <- suppressWarnings(pairs_from_delim(raw))
  expect_equal(length(unique(pairs$cvr_id)), n + 2L)
})
