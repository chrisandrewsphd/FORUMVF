#' Extract point information from FORUM XML file contain data from a 30-2 test
#'
#' @param top Root node of an xml file
#' @param asvector Determine whether the output is returned as a vector (TRUE, default) or as a matrix (FALSE).
#' @param eyeformat Determine the ordering of the response vector/matrix. If OD (default), the responses are ordered from top left to bottom right as they would appear on the paper printout for a right eye. If the xml file corresponds to a right eye, no transformation is done. If the xml file corresponds to a left eye, the locations are flipped across the y-axis. If eyeformat is OS, the responses are ordered from top left to bottom right as they would appear on the paper printout for left eye. If eyeformat is "file", the responses are ordered top left to bottom right without reference to eye. If eyeformat is "raw", the responses are in the order in which they were stored in the xml file.
#' @param dropXY Should the values of X and Y be included in the output for each point. The values of X and Y are standard for a given test pattern and eye laterality. For eyeformats OD and OS, the default is to drop X and Y. For eyeformats file and raw, the default is to keep X and Y.
#' @param verbose Control level of printing to console. The default (0) is no printing; larger values may produce more printing.
#'
#' @return a character vector with about 60 components extracted from the XML
#' @export
#'
#' @examples
#'    #exdatadir <- system.file('extdata**find new file', package = 'FORUMVF')
#'    #parsed <- xml2::read_xml(sprintf("%s/testdata.xml", exdatadir))
#'    #root <- xml2::xml_root(parsed)
#'    #xml_points30dash2(root)
xml_points30dash2 <- function(
    top,
    asvector = TRUE,
    eyeformat = "OD",
    dropXY = eyeformat %in% c("OD", "OS"),
    verbose = 0) {

  eyeformat <- match.arg(eyeformat, choices = c("OD", "OS", "file", "raw"))

  if (!("xml_node" %in% class(top))) stop("'top' must be class 'xml_node'.")

  ###########
  # TEST ID #
  ###########
  # ( 0) TestID: 00020003
  TestID <- text_of_first(top, '00020003')

  # check strategy
  strategysequence <- xml2::xml_find_first(top, ".//attr [@tag = '00400260']") # node with 2 children (test pattern, test strategy)
  # TestPattern <- xml2::xml_text(xml2::xml_find_first(xml2::xml_child(strategysequence, 1), ".//attr [@tag = '00080104']"))
  TestPattern <- text_of_first(xml2::xml_child(strategysequence, 1), '00080104')
  # TestStrategy <- xml2::xml_text(xml2::xml_find_first(xml2::xml_child(strategysequence, 2), ".//attr [@tag = '00080104']"))
  TestStrategy <- text_of_first(xml2::xml_child(strategysequence, 2), '00080104')

  if (verbose > 0) {
    cat(sprintf("Test Pattern and Strategy: '%s', '%s'\n", TestPattern, TestStrategy))
  }

  if (TestPattern != "Visual Field 30-2 Test Pattern") {
    if (verbose > 1) cat(sprintf("Use different function for pattern %s.\n", TestPattern))
    return(NULL)
    # return(
    #   if (isTRUE(asvector)) character(54 * 5 or 7 or 9 or 11)
    #   else matrix("", nrow = 5 or 7 or 9 or 11, ncol = 54))
  }

  # ( 5) Laterality: 00200060 and 00240113:Measurement Laterality
  Laterality1 <- text_of_first(top, '00200060')
  Laterality2 <- text_of_first(top, '00240113')

  if (verbose > 0) {
    cat(sprintf("Laterality 1 and 2: '%s', '%s'\n", Laterality1, Laterality2))
  }

  # node with 76 children
  pointsequence <- xml2::xml_find_first(top, ".//attr [@tag = '00240089']")
  if (xml2::xml_length(pointsequence) != 76L) {
    cat(sprintf("%3d points\n", xml2::xml_length(pointsequence)))
    stop("Pattern 30-2 but not 76 points.")
  }

  # extract information to matrix
  # nrow = 7 or 11
  # ncol = 76
  ## ***check xml_point applies to 30-2 ***
  pointmatrix <- sapply(seq.int(76L), FUN = function(i)
    xml_point(xml2::xml_child(pointsequence, i)))

  #########
  # ORDER #
  #########
  # order from top left to bottom right by horizontal rows

  reorder <- if (((eyeformat == "OD") && (Laterality1 == "R")) || ((eyeformat == "OS") && (Laterality1 == "L"))) {
    # no flip
    order(
      as.numeric(pointmatrix["Y", ]),
      as.numeric(pointmatrix["X", ]),
      method = "radix",
      decreasing = c(TRUE, FALSE))
  } else if (((eyeformat == "OD") && (Laterality1 == "L")) || ((eyeformat == "OS") && (Laterality1 == "R"))) {
    # flip
    order(
      as.numeric(pointmatrix["Y", ]),
      as.numeric(pointmatrix["X", ]),
      method = "radix",
      decreasing = c(TRUE, TRUE))
  } else if (eyeformat == "file") {
    # no flip
    order(
      as.numeric(pointmatrix["Y", ]),
      as.numeric(pointmatrix["X", ]),
      method = "radix",
      decreasing = c(TRUE, FALSE))
  } else if (eyeformat == "raw") {
    seq.int(ncol(pointmatrix))
  } else {
    print(eyeformat)
    print(Laterality1)
    stop("What?")
  }
  pointmatrix <- pointmatrix[, reorder]

  if (isTRUE(dropXY)) {
    # pointmatrix <- pointmatrix[-c(1, 2), ]
    pointmatrix <- pointmatrix[setdiff(rownames(pointmatrix), c("X", "Y")), ]
  }

  retval <- if (isTRUE(asvector)) {
    c(TestID = TestID, Eye = Laterality1, Format = eyeformat,
      as.vector(t(pointmatrix)))
  } else if (isFALSE(asvector)) {
    attr(pointmatrix, "TestID") <- TestID
    attr(pointmatrix, "Eye") <- Laterality1
    attr(pointmatrix, "Format") <- eyeformat
    pointmatrix
  } else stop("'asvector' must be TRUE or FALSE.")

  return(retval)
}

#we'll delete this. xml_point will be its own file. comment it out
# xml_point <- function(
#     pointroot,
#     extra4fields = FALSE) {
#   retvalnames <- if (isTRUE(extra4fields)) {
#     # c("X", "Y", "Stim Res", "Sens Val", "Stim Res 2", "Sens Val 2", "ACSDV", "ACSDPV", "GDCSDF", "GDCSDV", "GDCSDPV")
#     c("X", "Y", "Sens Val", "ACSDV", "GDCSDV", "ACSDPV", "GDCSDPV", "Stim Res", "Stim Res 2", "Sens Val 2", "GDCSDF")
#   } else if (isFALSE(extra4fields)) {
#     c("X", "Y", "Sens Val", "ACSDV", "GDCSDV", "ACSDPV", "GDCSDPV")
#   } else stop("extra4fields must be TRUE or FALSE")
#
#   retval <- character(length(retvalnames))
#   names(retval) <- retvalnames
#
#   retval["X"] <- text_of_first(pointroot, "00240090") # x-coordinate
#   retval["Y"] <- text_of_first(pointroot, "00240091") # y-coordinate
#   retval["Sens Val"] <- text_of_first(pointroot, "00240094") # Sensitivity value
#
#   # Normal Sequence
#   # Are the normalized values present?
#   normalparent <- xml2::xml_find_first(pointroot, ".//attr [@tag = '00240097']")
#   if (!is.na(normalparent)) {
#     normalroot <- xml2::xml_child(normalparent, 1)
#
#     retval["ACSDV"] <- text_of_first(normalroot, "00240092") # Age Corrected Sensitivity Deviation Value
#     retval["ACSDPV"] <- text_of_first(normalroot, "00240100") # Age Corrected Sensitivity Deviation Probability Value
#     retval["GDCSDV"] <- text_of_first(normalroot, "00240103") # Generalized Defect Corrected Sensitivity Deviation Value
#     retval["GDCSDPV"] <- text_of_first(normalroot, "00240104") # Generalized Defect Corrected Sensitivity Deviation Probability Value
#   } else {
#     retval[c("ACSDV", "ACSDPV", "GDCSDV", "GDCSDPV")] <- NA_character_
#   }
#
#   if (isTRUE(extra4fields)) {
#     # RETEST
#     retval["Stim Res"] <- text_of_first(pointroot, "00240093") # Stimulus result: SEEN vs NOT SEEN
#     retval["Stim Res 2"] <- text_of_first(pointroot, "00240095") # Stimulus result: SEEN vs NOT SEEN
#     retval["Sens Val 2"] <- text_of_first(pointroot, "00240096") # Sensitivity value
#     retval["GDCSDF"] <- if (!is.na(normalparent)) {
#       text_of_first(normalroot, "00240102") # Generalized Defect Corrected Sensitivity Deviation Flag
#     } else {
#       NA_character_
#     }
#   }
#
#   return(retval)
# }


