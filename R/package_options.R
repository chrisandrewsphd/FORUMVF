# Initialize a package variable used in the utility functions
the <- new.env(parent = emptyenv())

#' Change hdrtxt to package default
#' @export
reset_hdrtxt <- function() {
  old <- the$hdrtxt
  # the$hdrtxt <- "attr" # version 1
  the$hdrtxt <- "DicomAttribute" # version 2
  return(invisible(old))
}

reset_hdrtxt()

#' Retrieve hdrtxt
#' @export
get_hdrtxt <- function() {
  return(the$hdrtxt)
}

#' Change hdrtxt
#'
#' @param new_hdrtxt New text to be used in XML parsing
#'
#' @export
set_hdrtxt <- function(new_hdrtxt) {
  # currently no sanity checks on this
  old <- the$hdrtxt
  the$hdrtxt <- new_hdrtxt
  return(invisible(old))
}

