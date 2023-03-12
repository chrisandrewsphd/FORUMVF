#' Extract point information from FORUM XML file contain data from a 24-2 test
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
#'    parsed <- xml2::read_xml("testdata.xml")
#'    root <- xml2::xml_root(parsed)
#'    xml_points24dash2(root)
xml_points24dash2 <- function(
    top,
    asvector = TRUE,
    eyeformat = "OD",
    dropXY = eyeformat %in% c("OD", "OS"),
    verbose = 0) {

  eyeformat <- match.arg(eyeformat, choices = c("OD", "OS", "file", "raw"))

  # ( 5) Laterality: 00200060 and 00240113:Measurement Laterality
  # Laterality (for each tag) appeared at most once in sample of 10K files
  # Record first of each
  # els <- xml_find_all(top, ".//attr [@tag = '00200060']")
  # sapply(els, xml_text)
  # ( 5)* Laterality:
  # els <- xml_find_all(top, ".//attr [@tag = '00240113']")
  # sapply(els, xml_text)
  Laterality1 <- text_of_first(top, '00200060')
  Laterality2 <- text_of_first(top, '00240113')

  if (verbose > 0) {
    print(Laterality1)
    print(Laterality2)
  }


  # check strategy
  strategysequence <- xml2::xml_find_first(top, ".//attr [@tag = '00400260']") # node with 2 children (test pattern, test strategy)
  TestPattern <- xml2::xml_text(xml2::xml_find_first(xml2::xml_child(strategysequence, 1), ".//attr [@tag = '00080104']"))
  TestStrategy <- xml2::xml_text(xml2::xml_find_first(xml2::xml_child(strategysequence, 2), ".//attr [@tag = '00080104']"))

  if (verbose > 0) {
    print(TestPattern)
    print(TestStrategy)
  }

  if (TestPattern != "Visual Field 24-2 Test Pattern") {
    if (verbose > 1) cat(sprintf("Use different function for pattern %s.\n", TestPattern))
    return(NULL)
    # return(
    #   if (isTRUE(asvector)) character(54 * 5 or 7 or 9 or 11)
    #   else matrix("", nrow = 5 or 7 or 9 or 11, ncol = 54))
  }

  # node with 54 children
  pointsequence <- xml2::xml_find_first(top, ".//attr [@tag = '00240089']")
  if (xml_length(pointsequence) != 54L) {
    cat(sprintf("%3d points\n", xml_length(pointsequence)))
    stop("Pattern 24-2 but not 54 points.")
  }

  # extract information to matrix
  # nrow = 7 or 11
  # ncol = 54
  pointmatrix <- sapply(seq.int(54L), FUN = function(i)
    xml_point(xml_child(pointsequence, i)))

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
    pointmatrix <- pointmatrix[-c(1, 2), ]
  }

  retval <- if (isTRUE(asvector)) {
    as.vector(pointmatrix)
  } else if (isFALSE(asvector)) {
    pointmatrix
  } else stop("asvector must be TRUE or FALSE")

  return(retval)
}

xml_point <- function(
    pointroot,
    allfields = FALSE) {
  retvalnames <- if (isTRUE(allfields)) {
    c("X", "Y", "Stim Res", "Sens Val", "Stim Res 2", "Sens Val 2", "ACSDV", "ACSDPV", "GDCSDF", "GDCSDV", "GDCSDPV")
  } else if (isFALSE(allfields)) {
    c("X", "Y", "Sens Val", "ACSDV", "ACSDPV", "GDCSDV", "GDCSDPV")
  } else stop("allfields must be TRUE or FALSE")

  retval <- character(length(retvalnames))
  names(retval) <- retvalnames

  retval["X"] <- text_of_first(pointroot, "00240090") # x-coordinate
  retval["Y"] <- text_of_first(pointroot, "00240091") # y-coordinate
  retval["Sens Val"] <- text_of_first(pointroot, "00240094") # Sensitivity value

  # Normal Sequence
  normalroot <- xml_child(xml_find_first(pointroot, ".//attr [@tag = '00240097']"), 1)

  retval["ACSDV"] <- text_of_first(normalroot, "00240092") # Age Corrected Sensitivity Deviation Value
  retval["ACSDPV"] <- text_of_first(normalroot, "00240100") # Age Corrected Sensitivity Deviation Probability Value
  retval["GDCSDV"] <- text_of_first(normalroot, "00240103") # Generalized Defect Corrected Sensitivity Deviation Value
  retval["GDCSDPV"] <- text_of_first(normalroot, "00240104") # Generalized Defect Corrected Sensitivity Deviation Probability Value

  if (isTRUE(allfields)) {
    # RETEST
    retval[ 3] <- text_of_first(pointroot, "00240093") # Stimulus result: SEEN vs NOT SEEN
    retval[ 5] <- text_of_first(pointroot, "00240095") # Stimulus result: SEEN vs NOT SEEN
    retval[ 6] <- text_of_first(pointroot, "00240096") # Sensitivity value
    retval[ 9] <- text_of_first(normalroot, "00240102") # Generalized Defect Corrected Sensitivity Deviation Flag
  }

  return(retval)
}

# not for export
text_of_first <- function(node, tag) {
  # returns NA if node is missing
  if (is.na(node)) return(NA_character_)
  # returns NA if tag is not found
  # returns "" if tag is found but has no value
  xml2::xml_text(xml2::xml_find_first(node, sprintf(".//attr [@tag = '%s']", tag)))
}

plot_visual_field <- function(
    mat, valrow = 4,
    add = FALSE, probs = FALSE, textcolor = "black", strip.0 = TRUE,
    usedatarange = FALSE,
    format = "XY") {

  format <- match.arg(format, c("XY", "24-2 OD", "24-2 OS"))

  if (format == "XY") {
    xx <- as.numeric(mat["X", ])
    yy <- as.numeric(mat["Y", ])
  } else if (format == "24-2 OD") {
    xx <- c(seq(-9, 9, 6), seq(-15, 15, 6), seq(-21, 21, 6), seq(-27, 21, 6),
            seq(-27, 21, 6), seq(-21, 21, 6), seq(-15, 15, 6), seq(-9, 9, 6))
    yy <- rep(c(21, 15, 9, 3, -3, -9, -15, -21), times = c(4, 6, 8, 9, 9, 8, 6, 4))
  } else if (format == "24-2 OS") {
    xx <- c(seq(-9, 9, 6), seq(-15, 15, 6), seq(-21, 21, 6), seq(-21, 27, 6),
            seq(-21, 27, 6), seq(-21, 21, 6), seq(-15, 15, 6), seq(-9, 9, 6))
    yy <- rep(c(21, 15, 9, 3, -3, -9, -15, -21), times = c(4, 6, 8, 9, 9, 8, 6, 4))
  }

  vv <- mat[valrow, ]

  if (isTRUE(usedatarange)) {
    xlim <- range(xx)
    ylim <- range(yy)
  } else if (isFALSE(usedatarange)) {
    xlim <- c(-30, 30)
    ylim <- c(-30, 30)
  } else stop("usedatarange must be TRUE or FALSE")

  if (isFALSE(add)) {
    plot(
      NA, asp = 1, bty = "n",
      xlim = xlim, xlab = "", xaxt = "n",
      ylim = ylim, ylab = "", yaxt = "n")
    axis(1, at = seq(-30, 30, 10), labels = FALSE, pos = 0)
    axis(2, at = seq(-30, 30, 10), labels = FALSE, pos = 0)
  }

  if (isFALSE(probs)) text(
    xx, yy,
    if (isTRUE(strip.0)) gsub(".0", "", vv, fixed = TRUE) else vv,
    col = textcolor)
  else if (isTRUE(probs)) points(
    xx, yy, cex = 4, pch = 16,
    col = grey(c(0.1, 0.3, 0.5, 0.7, 0.9))[
      as.numeric(factor(
        vv,
        levels = c("0.5", "1.0", "2.0", "5.0", "0.0")))]
  )
  else stop("probs must be TRUE or FALSE")

  return(invisible(NULL))
}

# vfmat <- sapply(1:54, FUN = function(i) xml_point(xml_child(el, i)))
# dim(vfmat)
#
# plot_visual_field(vfmat)
# plot_visual_field(vfmat, "Sens Val")
# plot_visual_field(vfmat, "ACSDV")
# plot_visual_field(vfmat, "GDCSDV")
#
# plot_visual_field(vfmat, "ACSDPV", probs = TRUE)
# plot_visual_field(vfmat, "GDCSDPV", probs = TRUE)
#
# par(mar = c(1,1,1,1), las = 1, pty = "s")
# plot_visual_field(vfmat, "GDCSDPV", probs = TRUE)
# plot_visual_field(vfmat, "GDCSDV", probs = FALSE, add = TRUE, textcolor = "red")
# abline(h = 0, v = 0)
