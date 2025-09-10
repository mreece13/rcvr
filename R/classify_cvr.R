#' Classifies CVRs
#'
#' @param path (character) Path to either a single file CVR or to a directory containing many CVRs (e.g., a folder of JSON or XML files)
#' @param verbose (boolean) Controls verbosity of information printed to the user
#'
#' @returns (character) One of DELIM, JSON, or XML
classify_cvr <- function(path, verbose = TRUE) {
  checkmate::assert_character(path)

  path <- fs::path_real(path)

  if (verbose) cli::cli_alert("Processing {.file {path}}")

  # work with files first
  if (fs::is_file(path)) {
    header <- is_header(path)
    ext <- fs::path_ext(path) |> stringr::str_to_upper()

    if (!(ext %in% c("CSV", "XLS", "XLSX"))) {
      cli::cli_abort("{.var path} is a single file but it's neither a CSV or Excel file so we're not sure how to handle it")
    }

    if (verbose) {
      d <- switch(ext,
        "CSV" = data.table::fread(path, colClasses = character(), fill = TRUE),
        "XLS" = readxl::read_excel(path, col_types = "text", .name_repair = "unique_quiet"),
        "XLSX" = readxl::read_excel(path, col_types = "text", .name_repair = "unique_quiet"),
      )

      cli::cli({
        cli::cli_h2("File Information")
        cli::cli_dl(c(
          "File type" = ext,
          "Additional Header?" = header,
          "Number of rows" = nrow(d)
        ))
      })
    }

    return("DELIM")
  } else if (fs::is_dir(path)) {
    files <- list.files(path, recursive = TRUE)

    n_files = length(files)
    n_json <- sum(stringr::str_detect(files, "\\.json$"))
    n_xml <- sum(stringr::str_detect(files, "\\.xml$"))
    n_xlsx <- sum(stringr::str_detect(files, "\\.xls$|\\.xlsx$"))
    n_csv <- sum(stringr::str_detect(files, "\\.csv$"))

    if (n_files == 0) cli::cli_abort("No files in folder, perhaps you passed the wrong dir?")

    if (sum(c(n_json, n_xml, n_xlsx, n_csv) > 0) > 1) cli::cli_abort("Multiple file types detected, unclear what parser to use")

    if (n_xlsx > 0 & verbose) cli::cli_alert("{n_xlsx} Excel files found")
    if (n_csv > 0 & verbose) cli::cli_alert("{n_csv} CSV files found")
    if (n_json > 0 & verbose) cli::cli_alert("{n_json} JSON files found")
    if (n_xml > 0 & verbose) cli::cli_alert("{n_xml} XML files found")

    if (n_xlsx == 1 | n_csv == 1) return(list(type="DELIM",path=files[1]))
    if (n_xlsx > 1 | n_csv > 1) return("DELIM-MULTI")
    if (n_json > 0) return("JSON")
    if (n_xml > 0) return("XML")

  } else {
    cli::cli_abort("{.var path} passed is neither a file nor a directory")
  }
}
