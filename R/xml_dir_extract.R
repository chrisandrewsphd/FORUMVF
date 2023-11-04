#' Extract visual field values from all xml files in a directory
#'
#' @param xmldir (character) Name of directory containing xml files from FORUM
#' @param recursive (logical) Should sub directories be included? Default is FALSE
#' @param csvdir (character) Name of directory to write csv file(s) to. Default (\code{NULL}) is not to write files.
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
    comment = FALSE,
    verbose = 0) {

  if (is.null(csvdir)) {
    if (verbose > 0) cat("'csvdir' not specified. CSV data will not be exported/written.\n")
  } else if (length(csvdir) != 1) {
    cat("'csvdir' must be NULL or have length 1. CSV data will not be exported/written.\n")
    csvdir <- NULL
  } else if (isTRUE(dir.exists(csvdir))) {
    if (verbose > 0) cat(sprintf("CSV data will be written to %s\n", csvdir))
  } else {
    cat(sprintf("Directory %s does not exist. CSV data will not be exported/written\n.", csvdir))
    csvdir <- NULL
  }

  # all xml filenames in directory xmldir
  xmlfilenames <- grep(
    "\\.xml$",
    dir(xmldir, full.names = TRUE, recursive = recursive),
    value = TRUE)

  if (isTRUE(verbose > 0)) {
    cat(sprintf("%d filenames found in %s.\n", length(xmlfilenames), xmldir))
    if (isTRUE(verbose > 1)) {
      print(utils::head(xmlfilenames))
    }
  }

  # list of xml documents
  startTime <- Sys.time()
  if (isTRUE(verbose > 0)) {
    cat(sprintf("Started reading at %s\n", startTime))
  }
  st1 <-  system.time(
    list_parsed <- sapply(
      xmlfilenames,
      xml2::read_xml,
      simplify = FALSE,
      USE.NAMES = TRUE))
  if (isTRUE(verbose > 0)) {
    cat("Reading Duration", st1[1:3], "\n")
  }

  # list of root nodes
  list_top <- sapply(
    list_parsed,
    xml2::xml_root,
    simplify = FALSE,
    USE.NAMES = TRUE)

  # character matrix
  # nrows = number of files
  # ncolumns ~ 70
  st2 <- system.time(
    mat <- t(sapply(list_top, xml_extract, comment = comment)))
  if (isTRUE(verbose > 0)) {
    cat("Processing Duration", st2[1:3], "\n")
  }

  if (!is.null(csvdir)) {
    utils::write.csv(
      mat,
      file = sprintf("%s/OPH_HVF.csv", csvdir),
      row.names = FALSE,
      na = "")
  }

  return(invisible(mat))
}
