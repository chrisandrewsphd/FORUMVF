#' Extract data from XML node representing HVF point
#'
#' @param pointroot An xml node containing information about one point in a Humphrey Visual Field
#' @param extra4fields Most points have 7 pieces of information that are extracted. There are 4 other fields that rarely occur.  Should these extra 4 fields (Stimulus result: SEEN vs NOT SEEN, twice; Sensitivity value, and Generalized Defect Corrected Sensitivity Deviation Flag) be extracted also? Default is \code{FALSE}.
#'
#' @return A named character vector of length 7 (or 11) with names "X" (x-coordinate), "Y" (y-coordinate), "Sens Val" (sensitivity value), "ACSDV" (age corrected sensitivity deviation value), "GDCSDV" (generalized defect corrected sensitivity deviation value), "ACSDPV" (age corrected sensitivity deviation probability value), and "GDCSDPV" (generalized defect corrected sensitivity deviation probability value). Optionally: "Stim Res" (Stimulus result: SEEN vs NOT SEEN), "Stim Res 2" (Stimulus result: SEEN vs NOT SEEN), "Sens Val 2" (Sensitivity value), and "GDCSDF" (Generalized Defect Corrected Sensitivity Deviation Flag).
xml_point <- function(
    pointroot,
    extra4fields = FALSE) {
  retvalnames <- if (isTRUE(extra4fields)) {
    # c("X", "Y", "Stim Res", "Sens Val", "Stim Res 2", "Sens Val 2", "ACSDV", "ACSDPV", "GDCSDF", "GDCSDV", "GDCSDPV")
    c("X", "Y", "Sens Val", "ACSDV", "GDCSDV", "ACSDPV", "GDCSDPV", "Stim Res", "Stim Res 2", "Sens Val 2", "GDCSDF")
  } else if (isFALSE(extra4fields)) {
    c("X", "Y", "Sens Val", "ACSDV", "GDCSDV", "ACSDPV", "GDCSDPV")
  } else stop("extra4fields must be TRUE or FALSE")

  retval <- character(length(retvalnames))
  names(retval) <- retvalnames

  retval["X"] <- text_of_first(pointroot, "00240090") # x-coordinate
  retval["Y"] <- text_of_first(pointroot, "00240091") # y-coordinate
  retval["Sens Val"] <- text_of_first(pointroot, "00240094") # Sensitivity value

  # Normal Sequence
  # Are the normalized values present?
  # normalparent <- xml2::xml_find_first(pointroot, ".//attr [@tag = '00240097']")
  normalparent <- my_find_first(pointroot, '00240097')
  if (!is.na(normalparent)) {
    normalroot <- xml2::xml_child(normalparent, 1)

    retval["ACSDV"] <- text_of_first(normalroot, "00240092") # Age Corrected Sensitivity Deviation Value
    retval["ACSDPV"] <- text_of_first(normalroot, "00240100") # Age Corrected Sensitivity Deviation Probability Value
    retval["GDCSDV"] <- text_of_first(normalroot, "00240103") # Generalized Defect Corrected Sensitivity Deviation Value
    retval["GDCSDPV"] <- text_of_first(normalroot, "00240104") # Generalized Defect Corrected Sensitivity Deviation Probability Value
  } else {
    retval[c("ACSDV", "ACSDPV", "GDCSDV", "GDCSDPV")] <- NA_character_
  }

  if (isTRUE(extra4fields)) {
    # RETEST
    retval["Stim Res"] <- text_of_first(pointroot, "00240093") # Stimulus result: SEEN vs NOT SEEN
    retval["Stim Res 2"] <- text_of_first(pointroot, "00240095") # Stimulus result: SEEN vs NOT SEEN
    retval["Sens Val 2"] <- text_of_first(pointroot, "00240096") # Sensitivity value
    retval["GDCSDF"] <- if (!is.na(normalparent)) {
      text_of_first(normalroot, "00240102") # Generalized Defect Corrected Sensitivity Deviation Flag
    } else {
      NA_character_
    }
  }
  return(retval)
}
