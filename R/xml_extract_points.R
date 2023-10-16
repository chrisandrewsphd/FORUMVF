#' Extract point information from FORUM XML file
#'
#' @param top Root node of an xml file
#' @param eyeformat Determine the ordering of the response vector/matrix. If OD (default), the responses are ordered from top left to bottom right as they would appear on the paper printout for a right eye. If the xml file corresponds to a right eye, no transformation is done. If the xml file corresponds to a left eye, the locations are flipped across the y-axis. If eyeformat is OS, the responses are ordered from top left to bottom right as they would appear on the paper printout for left eye. If eyeformat is "file", the responses are ordered top left to bottom right without reference to eye. If eyeformat is "raw", the responses are in the order in which they were stored in the xml file.
#' @param dropXY Should the values of X and Y be included in the output for each point. The values of X and Y are standard for a given test pattern and eye laterality. For eyeformats OD and OS, the default is to drop X and Y. For eyeformats file and raw, the default is to keep X and Y.
#' @param extra4fields Each X,Y point provides either 7 or 11 pieces of information.  Are all 11 desired (\code{TRUE}) or just 7 (\code{FALSE}, default).
#' @param asvector Determine whether the output is returned as a vector (\code{TRUE}, default) or as a matrix (FALSE). \code{FALSE} can be useful for debugging.
#' @param verbose Control level of printing to console. The default (0) is no printing; larger values may produce more printing.
#'
#' @return a character vector with 6 components extracted from the XML and all points
#' @export
#'
#' @examples
#'    exdatadir <- system.file('extdata', package = 'FORUMVF')
#'    parsed <- xml2::read_xml(sprintf("%s/testdata.xml", exdatadir))
#'    root <- xml2::xml_root(parsed)
#'    xml_points24dash2(root)
xml_extract_points <- function(
    top,
    eyeformat = "OD",
    dropXY = eyeformat %in% c("OD", "OS"),
    extra4fields = FALSE,
    asvector = TRUE,
    verbose = 0) {

  eyeformat <- match.arg(eyeformat, choices = c("OD", "OS", "file", "raw"))

  if (!("xml_node" %in% class(top))) stop("'top' must be class 'xml_node'.")

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

  npoints <- switch(
    TestPattern,
    "Visual Field 24-2 Test Pattern" = 54L,
    "Visual Field 10-2 Test Pattern" = 68L,
    "Visual Field 30-2 Test Pattern" = 76L,
    "Visual Field 60-4 Test Pattern" = NA_integer_,
    "Visual Field 24-2C Test Pattern" = NA_integer_,
    NA_integer_)

  if (is.na(npoints)) {
    if (verbose > 1) cat(sprintf("Use different function for pattern %s.\n", TestPattern))
    return(NULL)
  }

  # ( 5) Laterality: 00200060 and 00240113:Measurement Laterality
  Laterality1 <- text_of_first(top, '00200060')
  Laterality2 <- text_of_first(top, '00240113')

  if (verbose > 0) {
    cat(sprintf("Laterality 1 and 2: '%s', '%s'\n", Laterality1, Laterality2))
  }

  # node with 54/70/60/... children
  pointsequence <- xml2::xml_find_first(top, ".//attr [@tag = '00240089']")

  if (xml2::xml_length(pointsequence) != npoints) {
    cat(sprintf("%3d points\n", xml2::xml_length(pointsequence)))
    stop(sprintf("%s but %d points.", TestPattern, xml2::xml_length(pointsequence)))
  }

  # extract information to matrix
  # nrow = 7 or 11
  # ncol = npoints
  pointmatrix <- sapply(seq.int(npoints), FUN = function(i) {
    xml_point(
      pointroot = xml2::xml_child(pointsequence, i),
      extra4fields = extra4fields)
    })

  #########
  # ORDER #
  #########
  # order from top left to bottom right by horizontal rows

  reorder <- if (
    ((eyeformat == "OD") && (Laterality1 == "R")) ||
    ((eyeformat == "OS") && (Laterality1 == "L"))) {
    # no flip
    order(
      as.numeric(pointmatrix["Y", ]),
      as.numeric(pointmatrix["X", ]),
      method = "radix",
      decreasing = c(TRUE, FALSE))
  } else if (
    ((eyeformat == "OD") && (Laterality1 == "L")) ||
    ((eyeformat == "OS") && (Laterality1 == "R"))) {
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
