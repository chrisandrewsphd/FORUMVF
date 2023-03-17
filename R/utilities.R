#' Return the text value associated with a tag
#'
#' @param node xml node
#' @param tag attribute tag to find
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
      sprintf(".//attr [@tag = '%s']", tag)))
}

#' Return the text value associated with a tag
#'
#' @param node xml node
#' @param tag attribute tag to find
#'
#' @return length-1 character vector containing text of tag. Value is NA
#'     if node is missing or if tag is missing.  Value is "" if tag is found
#'     for node but the tag has no value.
text_of_most_common_with_check <- function(node, tag) {
  # returns NA if node is missing
  if (is.na(node)) return(NA_character_)

  if (!("xml_node" %in% class(node))) stop("'node' must be class 'xml_node'.")


  # nodeset of all matches (possible empty)
  els <- xml2::xml_find_all(node, sprintf(".//attr [@tag = '%s']", tag))

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
    # If not unique, issue warning and return most common (although ties are broken randomly)
    else {
      if (length(tab) > 1) warning(sprintf("Multiple values are not unique for tag %s.", tag))
      return(names(tab)[1])
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
#' @param textdate0 character vector of dates in format YYYYMMDD
#' @param textdate1 character vector of dates in format YYYYMMDD
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
