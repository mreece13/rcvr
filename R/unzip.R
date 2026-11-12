#' Extract a zipped CVR export to a temporary directory
#'
#' CVR exports are often kept zipped (expanded Dominion/Hart exports can hold
#' hundreds of thousands of small files, which sync badly). `clean_cvr()`
#' accepts a `.zip` path and calls this to extract only the CVR members --
#' `json`, `xml`, `csv`, `xls`, `xlsx` -- into a temporary directory, skipping
#' ballot images and PDFs.
#'
#' A zip whose only CVR content is one or more nested zips (an export of
#' exports: Hardin OH, Hamilton TN) is not unpacked recursively, since it is
#' ambiguous which inner export is meant. Name the inner export with `::`
#' instead: `"outer.zip::CVR_Export_20220825132943.zip"`. Any depth works
#' (`"a.zip::b.zip::c.zip"`); each inner name is the member path inside the
#' zip before it.
#'
#' If the extracted tree is a single top-level directory, that directory is
#' returned, so a Dominion export zipped with its folder resolves to the
#' folder holding the manifests.
#'
#' @param path Path to a `.zip` file, optionally followed by `::member.zip`
#'   segments naming a nested zip.
#' @param exdir Directory to extract into. Defaults to a fresh directory under
#'   `tempdir()`; the caller is responsible for deleting it.
#'
#' @return The path to read: the extraction directory, or its single
#'   top-level subdirectory.
#' @keywords internal
unzip_cvr <- function(path, exdir = tempfile("rcvr_zip_")) {
  # the unzip binary gets a shell-quoted path, so `~` would not be expanded
  parts <- path.expand(strsplit(path, "::", fixed = TRUE)[[1]])
  path <- paste(parts, collapse = "::")
  if (length(parts) > 1) {
    checkmate::assert_file_exists(parts[1])
    fs::dir_create(exdir)
    stage <- file.path(exdir, "nested")
    member <- parts[2]
    utils::unzip(parts[1], files = member, exdir = stage)
    inner <- file.path(stage, member)
    if (!file.exists(inner)) {
      cli::cli_abort(
        "{.file {parts[1]}} has no member {.file {member}}.",
        class = "rcvr_no_files"
      )
    }
    rest <- paste(c(inner, parts[-(1:2)]), collapse = "::")
    return(unzip_cvr(rest, exdir = file.path(exdir, "cvr")))
  }

  checkmate::assert_file_exists(path)
  fs::dir_create(exdir)

  exts <- c("json", "xml", "csv", "xls", "xlsx")
  patterns <- c(paste0("*.", exts), paste0("*.", toupper(exts)))

  unzip_bin <- Sys.which("unzip")
  if (nzchar(unzip_bin)) {
    # the system binary takes wildcard patterns, which matters for exports of
    # 500k+ members where passing every name to utils::unzip() is very slow.
    # Exit status 11 means "no matching files", handled below.
    status <- system2(
      unzip_bin,
      c("-q", "-o", shQuote(path), shQuote(patterns), "-x", shQuote("__MACOSX/*"), "-d", shQuote(exdir)),
      stdout = FALSE, stderr = FALSE
    )
    if (!status %in% c(0L, 11L)) {
      cli::cli_abort("{.code unzip} failed on {.file {path}} (exit status {status}).",
                     class = "rcvr_bad_zip")
    }
  } else {
    members <- utils::unzip(path, list = TRUE)$Name
    keep <- members[stringr::str_detect(members, stringr::regex(
      paste0("\\.(", paste(exts, collapse = "|"), ")$"), ignore_case = TRUE
    )) & !startsWith(members, "__MACOSX/")]
    if (length(keep) > 0) utils::unzip(path, files = keep, exdir = exdir)
  }

  files <- list.files(exdir, recursive = TRUE, all.files = FALSE)
  if (length(files) == 0) {
    inner <- utils::unzip(path, list = TRUE)$Name
    inner_zips <- inner[stringr::str_detect(inner, stringr::regex("\\.zip$", ignore_case = TRUE))]
    cli::cli_abort(
      c("{.file {path}} contains no CVR files (json, xml, csv, xls, xlsx).",
        if (length(inner_zips) > 0) c("i" = "It holds {length(inner_zips)} nested zip{?s}: {.file {utils::head(inner_zips, 5)}}. Point at one of them as {.code {paste0(basename(path), '::', inner_zips[1])}}.")),
      class = "rcvr_no_files"
    )
  }

  top <- list.dirs(exdir, recursive = FALSE)
  loose <- list.files(exdir, recursive = FALSE, include.dirs = FALSE)
  loose <- loose[!fs::is_dir(fs::path(exdir, loose))]
  if (length(top) == 1 && length(loose) == 0) top else exdir
}
