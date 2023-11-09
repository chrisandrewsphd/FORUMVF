#' Extract visual field summaries and point value from all xml files in a directory
#'
#' @param xmldir (character) Name of directory containing xml files from FORUM
#' @param recursive (logical) Should sub directories be included? Default is FALSE
#' @param csvdir (character) Name of directory to write csv file(s) to. Default (\code{NULL}) is not to write files.
#' @param comment (logical) Should text comment and patient comment be extracted? Default is FALSE.
#' @param eyeformat See `xml_extract_points()`. Leave as \code{"OD"} (default).
#' @param dropXY See `xml_extract_points()`.  Leave as \code{TRUE} (default).
#' @param extra4fields See `xml_extract_points()`.  Leave as \code{FALSE} (default).
#' @param verbose (integer) Amount of output to the console.  0 is none. Higher values may produce more output. Default 0.
#'
#' @return list with 3 components (invisibly). Component 1 ("main") is matrix of HVF summary data. Component 2 ("points") is a list of character matrices. Each subcomponent is a matrix for a Test Pattern containing the points data. Component 3 ("full") is the same as component 2 but includes both summary and points data.
#' @export
#'
#' @examples xml_dir_extract_full(".")
xml_dir_extract_full <- function(
    xmldir,
    recursive = FALSE,
    csvdir = NULL,
    comment = FALSE,
    eyeformat = "OD",
    dropXY = eyeformat %in% c("OD", "OS"),
    extra4fields = FALSE,
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

  # main
  # character matrix
  # nrows = number of files
  # ncolumns ~ 70
  st2 <- system.time(
    mat <- t(sapply(list_top, xml_extract, comment = comment)))
  if (isTRUE(verbose > 0)) {
    cat("Processing Duration", st2[1:3], "\n")
  }

  # points
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

  # mat and lst should have same length
  if (length(lst) != nrow(mat)) stop("Numbers of tests do not agree")

  lst2 <- lapply(
    seq_along(lst),
    FUN = function(i) {
      if (mat[i,"TestID"] != lst[[i]]["TestID"]) stop("TestID mismatch")
      if (mat[i,"TestPattern"] != lst[[i]]["TestPattern"]) stop("TestPattern mismatch")
      c(mat[i,], lst[[i]][-match(c("TestID", "TestPattern"), names(lst[[i]]))])
    }
  )

  # summarize test protocols
  tp <- sapply(lst, FUN = function(v) v["TestPattern"])
  testpatterns <- sort(unique(tp))
  if (verbose > 0) {
    cat(sprintf("%d unique test patterns reported\n", length(testpatterns)))
    if (verbose > 1) {
      print(table(tp))
    }
  }

  # collect by test protocol (list of matrices)
  lstlst <- split(lst, tp)
  lstmat <- lapply(lstlst, FUN = function(lst) do.call(rbind, lst))

  lstlst2 <- split(lst2, tp)
  lstmat2 <- lapply(lstlst2, FUN = function(lst) do.call(rbind, lst))

  # write to file?
  if (!is.null(csvdir)) {
    utils::write.csv(
      mat,
      file = sprintf("%s/OPH_HVF.csv", csvdir),
      row.names = FALSE,
      na = "")

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
        utils::write.csv(
          lstmat2[[testpattern]],
          file = sprintf(
            "%s/OPH_HVF_FULL_%s.csv",
            csvdir,
            gsub(" ", "_", testpattern)),
          row.names = FALSE,
          na = "")
      })
  }

  return(invisible(
    list(
      main = mat,
      points = lstmat,
      full = lstmat2)))
}
