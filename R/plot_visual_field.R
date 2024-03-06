#' Plot an HVF based on extracted matrix or vector.
#'
#' @param mat A character matrix containing HVF test data, typically from \code{xml_extract_points(..., asvector = FALSE)}; a character vector, perhaps a subvector of \code{xml_extract_points(..., asvector = TRUE)}; or \code{NULL}, which will number the points of the specified \code{testpattern}: one of "24-2", "24-2C", "10-2", "30-2", and "60-4".
#' @param valrow If \code{mat} is a matrix, the name of the row in \code{mat} containing the values to plot. If \code{mat} is a vector, \code{valrow} is ignored.
#' @param testpattern Format of data provided. The default ("XY") will plot everything based on rows "X" and "Y" of \code{mat}. If X and Y are not part of \code{mat}, the coordinates must be inferred from the test pattern. Possibilities currently are "24-2", "24-2C", "10-2", "30-2", and "60-4".
#' @param imagelaterality Should the HVF be plotted as OD or OS? If \code{testpattern == "XY"}, the possible options are "match" (default), which uses the provided X and Y, or "flip", which flips the image horizontally across the vertical axis. If \code{testpattern != "XY"}, the possible options are "OD" and "OS" (or "match" (default), in which case \code{eyestorageformat} will be used, which is almost certainly "OD").
#' @param addtoplot Should the values be added to an existing plot (\code{TRUE}) or a new plot (\code{FALSE}, default)?
#' @param probs Are the values coded probabilities (\code{TRUE}) or VF scores (\code{FALSE}, default)?
#' @param textcolor Color of text to be plotted. Default is \code{"black"}.
#' @param strip.0 Should trailing ".0" be removed before plotting? Default is \code{TRUE}.
#' @param usedatarange Should the graph boundaries be determined by rows "X" and "Y" of the data (\code{TRUE}) or default values for typical display for the specified test pattern (\code{FALSE}, default)?
#' @param addlegend Should the graph have a p-value legend at bottom. Default is \code{TRUE} (only relevant if \code{probs} is \code{TRUE}).
#' @param adddegrees Should the graph have the axes labeled with degrees? Default is \code{FALSE}.
#' @param eyestorageformat The default storage order in FORUMVF is OD, regardless of which eye is the subject of the test. Do not change this to "OS" except in very unusual circumstances. The format stores data generally from left to right, top to bottom. For the default "OD", this is Nasal to Temporal, Superior to Inferior.
#'
#' @return invisible NULL. Plot produced as side effect.
#' @export
#'
#' @examples
#'    exdatadir <- system.file('extdata', package = 'FORUMVF')
#'    parsed <- xml2::read_xml(sprintf("%s/testdata.xml", exdatadir))
#'    root <- xml2::xml_root(parsed)
#'    vfmat <- xml_points24dash2(root, asvector = FALSE, dropXY = FALSE)
#'
#'    op <- par(mar = rep(1, 4))
#'    plot_visual_field(vfmat)
#'    plot_visual_field(vfmat, "ACSDV")
#'    plot_visual_field(vfmat, "GDCSDV")
#'    plot_visual_field(vfmat, "ACSDPV", probs = TRUE)
#'    plot_visual_field(vfmat, "GDCSDPV", probs = TRUE)
#'
#'    par(mar = c(1,1,1,1), las = 1, pty = "s")
#'    plot_visual_field(vfmat, "GDCSDPV", probs = TRUE)
#'    plot_visual_field(vfmat, "GDCSDV", probs = FALSE, addtoplot = TRUE, textcolor = "red")
#'    abline(h = 0, v = 0)
#'
#'    plot_visual_field(testpattern = "24-2")
#'    plot_visual_field(testpattern = "24-2", imagelaterality = "OS")
#'
#'    plot_visual_field(testpattern = "24-2C", textcolor = "red", adddegrees = TRUE)
#'    plot_visual_field(testpattern = "24-2", textcolor = "black", addtoplot = TRUE)
#'    legend("topright", fill = c("black", "red"), c("24-2", "24-2C"), bty = "n")
#'
#'    plot_visual_field(testpattern = "60-4", textcolor = "green", adddegrees = TRUE)
#'    plot_visual_field(testpattern = "30-2", textcolor = "blue", addtoplot = TRUE)
#'    plot_visual_field(testpattern = "10-2", textcolor = "purple", addtoplot = TRUE)
#'    legend("topright", fill = c("green", "blue", "purple"), c("60-4", "30-2", "10-2"), bty = "n")
#'
#'    par(op)
#'
plot_visual_field <- function(
    mat = NULL,
    valrow = "Sens Val",
    testpattern = c("XY", "24-2", "24-2C", "10-2", "30-2", "60-4"),
    imagelaterality = "match",
    addtoplot = FALSE, probs = FALSE, textcolor = "black", strip.0 = TRUE,
    usedatarange = FALSE,
    addlegend = TRUE,
    adddegrees = FALSE,
    eyestorageformat = "OD") {

  eyestorageformat <- match.arg(
    eyestorageformat,
    choices = c("OD", "OS"))
  if (eyestorageformat != "OD") warning("Not using OD eye storage format. Beware.")

  imagelaterality <- match.arg(
    imagelaterality,
    choices = c("match", "flip", "OD", "OS"))

  testpattern <- match.arg(testpattern)

  if (is.null(mat)) {
    if (testpattern == "XY") stop("if 'mat' is NULL, testpattern must be specified")
    mat <- seq.int(HVF_XY_defaults[[eyestorageformat]][[testpattern]][["npoints"]])
  }

  if (testpattern == "XY") {
    xx <- as.numeric(mat["X", ])
    if (imagelaterality == "flip") xx <- -xx
    yy <- as.numeric(mat["Y", ])
  } else if (testpattern %in% names(HVF_XY_defaults[[eyestorageformat]])) {
    xx <- HVF_XY_defaults[[eyestorageformat]][[testpattern]][["X"]]
    if ((imagelaterality != "match") &&
        (imagelaterality != eyestorageformat)) xx <- -xx
    yy <- HVF_XY_defaults[[eyestorageformat]][[testpattern]][["Y"]]
  } else {
    stop(sprintf("testpattern %s does not have XY defaults stored.", testpattern))
  }

  vv <- if (is.matrix(mat)) {
    mat[valrow, ]
  } else {
    mat
  }

  if (isTRUE(usedatarange)) {
    xlim <- range(xx)
    ylim <- range(yy)
  } else if (isFALSE(usedatarange)) {
    switch(
      testpattern,
      "24-2" = {
        xlim <- c(-30, 30)
        ylim <- c(-30, 30)
      },
      "24-2C" = {
        xlim <- c(-30, 30)
        ylim <- c(-30, 30)
      },
      "10-2" = {
        xlim <- c(-10, 10)
        ylim <- c(-10, 10)
      },
      "30-2" = {
        xlim <- c(-30, 30)
        ylim <- c(-30, 30)
      },
      "60-4" = {
        xlim <- c(-60, 60)
        ylim <- c(-60, 60)
      },
      "XY" = {
        xlim <- range(xx)
        ylim <- range(yy)
      },
      {
        warning(sprintf("testpattern %s does not have default data range defined. Using range of X and Y.", testpattern))
        xlim <- range(xx)
        ylim <- range(yy)
      }
    )
  } else stop("'usedatarange' must be TRUE or FALSE")

  # set up canvas (unless adding to an existing plot)
  if (isFALSE(addtoplot)) {
    plot(
      NA, asp = 1, bty = "n",
      xlim = xlim, xlab = "", xaxt = "n",
      ylim = ylim, ylab = "", yaxt = "n")
    for (i in c(1,2)) {
      graphics::axis(
        i, labels = isTRUE(adddegrees), pos = 0, cex.axis = 0.5, las = 1,
        mgp = c(0, 0, 0), hadj = 1,
        padj = if (i == 1) -1.0 else 0.0,
        at = switch(
          testpattern,
          "24-2" = seq(-27, 27, 6),
          "24-2C" = seq(-27, 27, 6),
          "30-2" = seq(-27, 27, 6),
          "10-2" = seq(-9, 9, 2),
          "60-4" = seq(-54, 54, 12),
          "XY" = NULL))
    }
  }

  # draw
  if (isFALSE(probs)) graphics::text(
    xx, yy,
    if (isTRUE(strip.0)) gsub(".0", "", vv, fixed = TRUE) else vv,
    col = textcolor)
  else if (isTRUE(probs)) {
    graphics::points(
      xx, yy, cex = 4, pch = 16,
      col = grDevices::grey(c(0.1, 0.3, 0.5, 0.7, 0.9))[
        as.numeric(factor(
          vv,
          levels = c("0.5", "1.0", "2.0", "5.0", "0.0")))]
    )
    if (isTRUE(addlegend)) {
      graphics::legend(
        "bottom",
        legend = c("<0.5%", "<1%", "<2%", "<5%", "NS"),
        pch = 16,
        col = grDevices::grey(c(0.1, 0.3, 0.5, 0.7, 0.9)),
        pt.cex = 2,
        # fill = grDevices::grey(c(0.1, 0.3, 0.5, 0.7, 0.9)),
        # border = NA,
        horiz = TRUE,
        bty = "n")
    }
  }
  else stop("'probs' must be TRUE or FALSE")

  return(invisible(NULL))
}

