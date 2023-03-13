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
  # returns NA if tag is not found
  # returns "" if tag is found but has no value
  xml2::xml_text(xml2::xml_find_first(node, sprintf(".//attr [@tag = '%s']", tag)))
}

