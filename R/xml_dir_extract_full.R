#' Extract visual field summaries and point value from all xml files in a directory
#'
#' @param xmldir (character) Name of directory containing xml files from FORUM.
#' @param recursive (logical) Should sub directories be included? Default is
#' \code{FALSE}.
#' @param csvdir (character) Name of directory to write csv file(s) to. Default (\code{NULL}) is not to write files.
#' @param excludenonhvf (logical) Should files that don't have a recognizable
#' HVF Test Pattern be excluded from the output file? If \code{FALSE} (default),
#' then the number of rows of the returned matrix will be the same as the number
#' of files in \code{xmldir}. If \code{TRUE}, it may be less. Files with empty
#' test pattern will be excluded.
#' @param comment (logical) Should text comment and patient comment be extracted? Default is \code{FALSE}.
#' @param eyeformat See `xml_extract_points()`. Leave as \code{"OD"} (default).
#' @param dropXY See `xml_extract_points()`.  Leave as \code{TRUE} (default).
#' @param extra4fields See `xml_extract_points()`.  Leave as \code{FALSE} (default).
#' @param verbose (integer) Amount of output to the console.  0 is none. Higher values may produce more output. Default 0.
#'
#' @return list of character matrices (invisibly). Each component is a
#' matrix for a Test Pattern containing the summary and points data.
#' @export
#'
#' @examples xml_dir_extract_full(".")
xml_dir_extract_full <- function(
    xmldir,
    recursive = FALSE,
    csvdir = NULL,
    excludenonhvf = FALSE,
    comment = FALSE,
    eyeformat = "OD",
    dropXY = eyeformat %in% c("OD", "OS"),
    extra4fields = FALSE,
    verbose = 0L) {

  csvdir <- csvdircheck(csvdir = csvdir, verbose = verbose)

  list_top <- xml_dir_explode(
    xmldir = xmldir,
    recursive = recursive,
    verbose = verbose)

  if (length(list_top) == 0) {
    cat(sprintf("no xml files found in %s\n", xmldir))
    return(invisible(NULL))
  }

  # list
  # nelements = number of files
  # ncolumns differs based on type of test
  st2 <- system.time(
    lst <- lapply(
      list_top,
      xml_extract_full,
      eyeformat = eyeformat,
      dropXY = dropXY,
      extra4fields = extra4fields,
      verbose = verbose))

  if (isTRUE(verbose > 0L)) {
    cat("Processing Duration", st2[1:3], "\n")
  }

  tp <- factor(
    sapply(lst, FUN = function(v) v["TestPattern"]),
    exclude = NULL)
  testpatterns <- sort(unique(tp), na.last = TRUE)

  if (isTRUE(verbose > 0L)) {
    cat(sprintf(
      "%d unique test patterns reported\n", length(testpatterns)))
    if (isTRUE(verbose > 1L)) {
      print(table(tp))
    }
  }
  lstlst <- split(lst, tp)

  if (isTRUE(excludenonhvf)) {
    lstlst[which(is.na(names(lstlst)))] <- NULL
    lstlst[which(names(lstlst) == "")] <- NULL
  }

  lstmat <- lapply(lstlst, FUN = function(lst) do.call(rbind, lst))

  # write to file?
  if (!is.null(csvdir)) {
    lapply(
      names(lstmat), # testpatterns,
      FUN = function(testpattern) {
        utils::write.csv(
          lstmat[[testpattern]],
          file = sprintf(
            "%s/OPH_HVF_FULL_%s.csv",
            csvdir,
            gsub(" ", "_", testpattern)),
          row.names = FALSE,
          na = "")
      })
  }

  return(invisible(lstmat))
}
