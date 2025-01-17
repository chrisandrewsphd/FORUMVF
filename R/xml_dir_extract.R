#' Extract visual field values from all xml files in a directory
#'
#' @param xmldir (character) Name of directory containing xml files from FORUM
#' @param recursive (logical) Should sub directories be included? Default is FALSE
#' @param csvdir (character) Name of directory to write csv file(s) to. Default (\code{NULL}) is not to write files.
#' @param excludenonhvf (logical) Should files that don't have a recognizable HVF Test Pattern be excluded from the output file? If \code{FALSE} (default), then the number of rows of the returned matrix will be the same as the number of files in \code{xmldir}. If \code{TRUE}, it may be less. Files with empty test pattern will be excluded.
#' @param comment (logical) Should text comment and patient comment be extracted? Default is FALSE.
#' @param verbose (integer) Amount of output to the console.  0 is none. Higher values may produce more output. Default 0.
#'
#' @return character matrix. One row per file.  First column is file name with complete path. Subsequent columns are extracted values from xml files.
#' @export
#'
#' @examples # xml_dir_extract(".") # files in current directory
xml_dir_extract <- function(
    xmldir,
    recursive = FALSE,
    csvdir = NULL,
    excludenonhvf = FALSE,
    comment = FALSE,
    verbose = 0) {

  csvdir <- csvdircheck(csvdir = csvdir, verbose = verbose)

  list_top <- xml_dir_explode(
    xmldir = xmldir,
    recursive = recursive,
    verbose = verbose)

  if (length(list_top) == 0) {
    cat(sprintf("no xml files found in %s\n", xmldir))
    return(invisible(NULL))
  }

  # character matrix
  # nrows = number of files
  # ncolumns ~ 70
  st2 <- system.time(
    mat <- t(sapply(
      list_top,
      xml_extract, comment = comment, verbose = verbose - 1)))

  if (isTRUE(verbose > 0)) {
    cat("Processing Duration", st2[1:3], "\n")
  }

  if (isTRUE(verbose > 0)) {
    cat("Distribution of Test Patterns:\n")
    print(table(mat[, "TestPattern"], useNA = 'always'))
  }

  if (isTRUE(excludenonhvf)) {
    # exclude rows for which TestPattern is "" or NA
    # i.e., include rows for which TestPattern is not missing and not ""
    mat <- mat[
      which(!is.na(mat[, "TestPattern"]) & (mat[, "TestPattern"] != "")),
      ,
      drop = FALSE]
    if (isTRUE(verbose > 0)) {
      cat(sprintf(
        "Data from %d of %d files kept (%d non-HVF excluded)\n",
        nrow(mat), length(list_top), length(list_top) - nrow(mat)))
    }
  }

  if (!is.null(csvdir)) {
    utils::write.csv(
      mat,
      file = sprintf("%s/%s.csv", csvdir, "OPH_HVF"),
      row.names = FALSE,
      na = "")
  }

  return(invisible(mat))
}
