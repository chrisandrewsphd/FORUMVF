#' Find root node for each xml file in a directory
#'
#' @param xmldir (character) Name of directory containing xml files from FORUM
#' @param recursive (logical) Should sub directories be included? Default is FALSE
#' @param verbose (integer) Amount of output to the console.  0 is none. Higher values may produce more output. Default 0.
#'
#' @return list of root nodes, one for each xml file in the directory xmldir
#' @export
#'
#' @examples xml_dir_explode(".")
xml_dir_explode <- function(
    xmldir,
    recursive = FALSE,
    verbose = 0) {

  # all xml filenames in directory xmldir
  # xmlfilenames <- grep(
  #   "\\.xml$",
  #   dir(xmldir, full.names = TRUE, recursive = recursive),
  #   value = TRUE)
  xmlfilenames <- dir(
    xmldir, full.names = TRUE, recursive = recursive,
    pattern = "\\.xml$", ignore.case = TRUE)

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

  return(list_top)
}
