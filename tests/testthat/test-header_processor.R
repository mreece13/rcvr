test_that("is_header detects the vendor election-title first cell", {
  expect_true(is_header(fixture_path("delim-header", "cvr.csv")))
  expect_false(is_header(fixture_path("delim-plain", "cvr.csv")))
})

test_that("header_processor concatenates the three header rows and drops them", {
  raw <- header_processor(fixture_path("delim-header", "cvr.csv"))

  expect_equal(nrow(raw), 2)
  expect_true(any(stringr::str_detect(colnames(raw), "^MAYOR\\|\\|JANE DOE\\|\\|DEM")))
})

test_that("the dedup suffix never reaches a key column", {
  pairs <- read_delim_cvr(fixture_path("delim-header", "cvr.csv")) |>
    pairs_from_delim()

  expect_setequal(pairs$contest, "MAYOR")
  expect_setequal(stats::na.omit(pairs$raw_party), c("DEM", "REP"))
  expect_false(any(stringr::str_detect(pairs$raw_party, "_\\d+$"), na.rm = TRUE))
  expect_false(any(stringr::str_detect(pairs$raw_candidate, "__RCVRDUP__")))
})

test_that("a candidate or party starting with NA keeps its NA", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "2022 General Election,,,,",
    "CvrNumber,Precinct,Attorney General (Vote For=1),Attorney General (Vote For=1),School Board (Vote For=2)",
    ",,NATHAN HOCHMAN,ROB BONTA,NANCY CAHILL",
    ",,REP,DEM,",
    "1,P1,1,0,1",
    "2,P1,0,1,0"
  ), f)
  pairs <- read_delim_cvr(f) |> pairs_from_delim()
  expect_setequal(unique(pairs$contest), c("Attorney General (Vote For=1)", "School Board (Vote For=2)"))
  expect_true(all(c("NATHAN HOCHMAN", "NANCY CAHILL") %in% pairs$raw_candidate))
})

test_that("a version number in the contest row's first cell is not a contest", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "2022 Mercer General Election,,,,",
    "5.15.16.1,,MAYOR (Vote For=1),MAYOR (Vote For=1),MAYOR (Vote For=1)",
    ",,JANE DOE,JOHN ROE,Write-in",
    "TabulatorNum,PrecinctPortion,DEM,REP,",
    "1,Ward 1,1,0,0",
    "2,Ward 2,0,1,0"
  ), f)
  pairs <- read_delim_cvr(f) |> pairs_from_delim()
  expect_setequal(unique(pairs$contest), "MAYOR (Vote For=1)")
  expect_setequal(unique(pairs$precinct), c("Ward 1", "Ward 2"))
})

test_that("a title naming no year is a header when the next cell is a Dominion version", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    '"Irwin Nov 3rd RECOUNT","5.5.12.1","","",""',
    '"","","","MAYOR (Vote For=1)","MAYOR (Vote For=1)"',
    '"","","","JANE DOE (Dem)","JOHN ROE (Rep)"',
    '"CvrNumber","PrecinctPortion","BallotType","",""',
    '="1","043-Ocilla","Ballot 3","1","0"',
    '="2","050-Holt","Ballot 3","0","1"'
  ), f)
  expect_true(is_header(f))
  pairs <- read_delim_cvr(f) |> pairs_from_delim()
  expect_setequal(unique(pairs$contest), "MAYOR (Vote For=1)")
  expect_setequal(unique(pairs$precinct), c("043-Ocilla", "050-Holt"))
  expect_false(is_header_cell("CvrNumber", "TabulatorNum"))
  expect_false(is_header_cell("", "5.5.12.1"))
})

test_that("an empty names row puts the contests in the first data row (Coos OR)", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Coos 2020,,,,",
    "dummy,,,,",
    ",,MAYOR,MAYOR,COUNCIL",
    "RowNumber,BallotStyleID,JANE DOE,JOHN ROE,ANN POE",
    "1,5,1,0,1",
    "2,5,0,1,0"
  ), f)
  raw <- read_delim_cvr(f)
  expect_equal(nrow(raw), 2)
  expect_true(all(c("RowNumber", "ballot_style") %in% colnames(raw)))
  pairs <- pairs_from_delim(raw)
  expect_setequal(unique(pairs$contest), c("MAYOR", "COUNCIL"))
  expect_setequal(unique(pairs$raw_candidate), c("JANE DOE", "JOHN ROE", "ANN POE", "undervote"))
})
