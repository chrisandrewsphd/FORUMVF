#' Return the first node found with the associated tag
#'
#' @param node xml node.
#' @param tag attribute tag to find (character).
#'
#' @return first node identified by tag
my_find_first <- function(node, tag) {
  # returns NA if node is missing
  if (is.na(node)) return(NA_character_)
  if (!("xml_node" %in% class(node))) stop("'node' must be class 'xml_node'.")
  # returns NA if tag is not found
  # returns "" if tag is found but has no value
  xml2::xml_find_first(
    node,
    sprintf(
      ".//%s [@tag = '%s']",
      the$hdrtxt,
      tag))
}

#' Return the all nodes found with the associated tag
#'
#' @param node xml node.
#' @param tag attribute tag to find (character).
#'
#' @return all nodes identified by tag
my_find_all <- function(node, tag) {
  # returns NA if node is missing
  if (is.na(node)) return(NA_character_)
  if (!("xml_node" %in% class(node))) stop("'node' must be class 'xml_node'.")
  # returns NA if tag is not found
  # returns "" if tag is found but has no value
  xml2::xml_find_all(
    node,
    sprintf(
      ".//%s [@tag = '%s']",
      the$hdrtxt,
      tag))
}

#' Return the text value associated with a tag
#'
#' @param node xml node.
#' @param tag attribute tag to find.
#'
#' @return length-1 character vector containing text of tag. Value is NA
#'     if node is missing or if tag is missing.  Value is "" if tag is found
#'     for node but the tag has no value.
text_of_first <- function(node, tag) {
  # returns NA if node is missing
  if (is.na(node)) return(NA_character_)
  if (!("xml_node" %in% class(node))) stop("'node' must be class 'xml_node'.")
  # returns NA if tag is not found
  # returns "" if tag is found but has no value
  xml2::xml_text(
    xml2::xml_find_first(
      node,
      sprintf(
        ".//%s [@tag = '%s']",
        the$hdrtxt,
        tag)))
}

#' Return the text value associated with a tag
#'
#' @param node xml node.
#' @param tag attribute tag to find.
#' @param verbose Control amount of output to console. Default is 0. Larger values may produce more output.
#'
#' @return length-1 character vector containing text of tag. Value is NA
#'     if node is missing or if tag is missing.  Value is "" if tag is found
#'     for node but the tag has no value.
text_of_most_common_with_check <- function(node, tag, verbose = 0L) {
  # returns NA if node is missing
  if (is.na(node)) return(NA_character_)

  if (!("xml_node" %in% class(node))) stop("'node' must be class 'xml_node'.")

  # nodeset of all matches (possible empty)
  els <- xml2::xml_find_all(
    node,
    sprintf(
      ".//%s [@tag = '%s']",
      the$hdrtxt,
      tag))
  if (verbose > 1) print(els)

  # return NA if no matches
  if (length(els) == 0) return(NA_character_)
  # return text of only node if length is 1
  else if (length(els) == 1) return(xml2::xml_text(els))
  else {
    # character vector of text of multiple matches
    vec <- xml2::xml_text(els)
    # table of non-empty, non-missing text, sorted from most to least common.
    tab <- sort(table(vec, exclude = c(NA, "")), decreasing = TRUE)
    # return "" if all values were blank or missing
    if (length(tab) < 1) return("")
    # If at least one value, return most common (although ties are broken randomly)
    # after issuing warning if more than one value
    else {
      if (length(tab) > 1) {
        # warning(sprintf("Multiple values are not unique for tag %s.", tag))
        if (verbose > 0) {
          cat(sprintf("Multiple values are not unique for tag %s.\n", tag))
          if (verbose > 1) {
            print(tab)
          }
        }
      }
    }
    return(names(tab)[1])
  }
}


#' Return the text value associated with a tag. Tailored for MRN finding.
#'
#' @param node xml node.
#' @param tag attribute tag to find. Default is '00100020'.
#' @param verbose Control amount of output to console. Default is 0. Larger values may produce more output.
#'
#' @return length-1 character vector containing text of tag. Value is NA
#'     if node is missing or if tag is missing.  Value is "" if tag is found
#'     for node but the tag has no value.
text_of_mrn <- function(node, tag = '00100020', verbose = 0L) {
  # returns NA if node is missing
  if (is.na(node)) return(NA_character_)

  if (!("xml_node" %in% class(node))) stop("'node' must be class 'xml_node'.")

  # nodeset of all matches (possible empty)
  els <- xml2::xml_find_all(
    node,
    sprintf(
      ".//%s [@tag = '%s']",
      the$hdrtxt,
      tag))
  if (verbose > 1) print(els)

  # return NA if no matches
  if (length(els) == 0) return(NA_character_)
  # return text of only node if length is 1
  else if (length(els) == 1) return(xml2::xml_text(els))
  else {
    # character vector of text of multiple matches
    vec <- xml2::xml_text(els)
    # table of non-empty, non-missing text, sorted from most to least common.
    tab <- sort(table(vec, exclude = c(NA, "")), decreasing = TRUE)
    # return "" if all values were blank or missing
    if (length(tab) < 1) return("")
    # return only value if exactly 1 value
    else if (length(tab) == 1) return(names(tab))
    # If more than one value, return most common
    # if tie, return value with #digits closest to 9
    # after issuing message if more than one value and verbose > 0
    else {
      # warning(sprintf("Multiple values are not unique for tag %s.", tag))
      if (verbose > 0) cat(sprintf("Multiple values are not unique for tag %s.", tag))
      if (verbose > 1) print(tab)

      tab1 <- tab[tab == tab[1]] # select all the most common
      if (length(tab1) == 1) {
        if (verbose > 0) cat("Choosing most common.\n")
        return(names(tab1))
      } else {
        if (verbose > 0) cat("Choosing by number of digits.\n")
        return(names(tab1)[order(abs(nchar(names(tab1)) - 9.1))[1]]) # give slight preference to 10 digits over 8 due to hyphens.
      }
    }
  }
}


#' Compute age. text input and output
#'
#' @param textdate0 length-1 character vector of dates in format YYYYMMDD
#' @param textdate1 length-1 character vector of dates in format YYYYMMDD
#'
#' @return character vector of integer ages (age at last birthday)
computeage_dep <- function(textdate0, textdate1) {
  # difference in years between 0 and 1
  # this is the age the person will turn on textdate1
  rough <- as.numeric(substr(textdate1, 1, 4)) - as.numeric(substr(textdate0, 1, 4))
  # If prior month1 < month0, bday not happened yet. Subtract 1 and return age.
  if (substr(textdate1, 5, 6) < substr(textdate0, 5, 6))
    return(as.character(rough-1))
  # If prior month1 > month0, bday happened. Return age.
  else if (substr(textdate1, 5, 6) > substr(textdate0, 5, 6))
    return(as.character(rough))
  # else, same month; check day of month
  if (substr(textdate1, 7, 8) < substr(textdate0, 7, 8))
    return(as.character(rough-1))
  else return(as.character(rough))
}


#' Compute age. text input and output
#'
#' @param textdate0 character vector of dates in format YYYYMMDD.
#' @param textdate1 character vector of dates in format YYYYMMDD.
#'
#' @return character vector of integer ages (age at last birthday)
computeage <- function(textdate0, textdate1) {
  # difference in years between 0 and 1
  # this is the age the person will turn on textdate1
  rough <- as.numeric(substr(textdate1, 1, 4)) - as.numeric(substr(textdate0, 1, 4))

  fine <- ifelse(
    # If prior month1 < month0, bday not happened yet. Subtract 1 and return age.
    substr(textdate1, 5, 6) < substr(textdate0, 5, 6),
    rough-1,
    ifelse(
      # If prior month1 > month0, bday happened. Return age.
      substr(textdate1, 5, 6) > substr(textdate0, 5, 6),
      rough,
      ifelse(
        # else, same month; check day of month
        substr(textdate1, 7, 8) < substr(textdate0, 7, 8),
        rough-1,
        rough)))

  return(as.character(fine))
}

#' Check form/existence of csvdir
#'
#' @param csvdir proposed existing directory to write to
#' (character vector of length 1).
#' @param verbose level of output to console.
#'
#' @return either NULL or character string of existing directory to write to
csvdircheck <- function(csvdir, verbose = 0L) {
  if (is.null(csvdir)) {
    if (verbose > 0) cat("'csvdir' not specified. CSV data will not be exported/written.\n")
    return(NULL)
  } else if (length(csvdir) != 1) {
    cat("'csvdir' must be NULL or have length 1. CSV data will not be exported/written.\n")
    return(NULL)
  } else if (isTRUE(dir.exists(csvdir))) {
    if (verbose > 0) cat(sprintf("CSV data will be written to %s\n", csvdir))
    return(csvdir)
  } else {
    cat(sprintf("Directory %s does not exist. CSV data will not be exported/written\n.", csvdir))
    return(NULL)
  }
}
