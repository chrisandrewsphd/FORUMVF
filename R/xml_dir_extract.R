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

  csvdir <- csvdircheck(csvdir = csvdir, verbose = verbose)

  list_top <- xml_dir_explode(
    xmldir = xmldir,
    recursive = recursive,
    verbose = verbose)

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
