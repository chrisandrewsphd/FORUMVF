#' Extract visual field values from all xml files in a directory
#'
#' @param xmldir (character) Name of directory containing xml files from FORUM
#' @param recursive (logical) Should sub directories be included? Default is FALSE
#' @param csvdir (character) Name of directory to write csv file(s) to. Default (\code{NULL}) is not to write files.
#' @param eyeformat See `xml_extract_points()`. Leave as \code{"OD"} (default).
#' @param dropXY See `xml_extract_points()`.  Leave as \code{TRUE} (default).
#' @param extra4fields See `xml_extract_points()`.  Leave as \code{FALSE} (default).
#' @param verbose (integer) Amount of output to the console.  0 is none. Higher values may produce more output. Default 0.
#'
#' @return list of character matrices. Each component is a matrix for a Test Pattern. Each row of a matrix is for one file.  First column is file name with complete path. Subsequent columns are extracted values from xml files.
#' @export
#'
#' @examples # xml_dir_extract_points(".") # files in current directory
xml_dir_extract_points <- function(
    xmldir,
    recursive = FALSE,
    csvdir = NULL,
    eyeformat = "OD",
    dropXY = eyeformat %in% c("OD", "OS"),
    extra4fields = FALSE,
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

  # list
  # nelements = number of files
  # ncolumns differs based on type of test
  st2 <- system.time(
    lst <- lapply(
      list_top, xml_extract_points,
      eyeformat = eyeformat,
      dropXY = dropXY,
      extra4fields = extra4fields,
      verbose = verbose))
  if (isTRUE(verbose > 0)) {
    cat("Processing Duration", st2[1:3], "\n")
  }

  # collect by test protocol
  # tp <- sapply(lst, FUN = function(v) {
  #   if (is.null(attr(v, "TestPattern"))) return("Unknown Pattern")
  #   else return(attr(v, "TestPattern"))
  # })
  tp <- sapply(lst, FUN = function(v) v["TestPattern"])
  testpatterns <- sort(unique(tp))
  if (verbose > 0) {
    cat(sprintf("%d unique test patterns reported\n", length(testpatterns)))
    if (verbose > 1) {
      print(table(tp))
    }
  }
  lstlst <- split(lst, tp)
  lstmat <- lapply(lstlst, FUN = function(lst) do.call(rbind, lst))

  if (!is.null(csvdir)) {
    lapply(
      testpatterns,
      FUN = function(testpattern) {
        if (testpattern == "Unknown Pattern") return(NULL)
        utils::write.csv(
          lstmat[[testpattern]],
          file = sprintf(
            "%s/OPH_HVF_POINTS_%s.csv",
            csvdir,
            gsub(" ", "_", testpattern)),
          row.names = FALSE,
          na = "")
      })
  }

  return(invisible(lstmat))
}
