#' Plot a HVF based on extracted matrix (or vector).
#'
#' @param mat character matrix containing 24-2 SITA Fast test
#' @param valrow Name of the row in mat containing the values to plot
#' @param add Should the values be added to an existing plot (TRUE) or a new plot (FALSE, default)?
#' @param probs Are the values coded probabilities (TRUE) or VF scores (FALSE, default)
#' @param textcolor Color of text to be plotted. Default is "black".
#' @param strip.0 Should trailing ".0" be removed before plotting? Default is TRUE.
#' @param usedatarange Should the graph boundaries be determined by rows "X" and "Y" of the data (TRUE) or default values for 24-2 display (FALSE, default).
#' @param addlegend Should the graph have p-value legend at bottom. Default is TRUE (only relevant if probs is TRUE).
#' @param format Format of test. Default ("XY") is to plot everything based on rows "X" and "Y" of mat.  Other possibilities are "24-2 OD" and "24-2 OS" that can be used when "X" and "Y" are not provided.
#'
#' @return invisible NULL. Plot produced as side effect.
#' @export
#'
#' @examples
#'    exdatadir <- system.file('extdata', package = 'FORUMVF')
#'    parsed <- xml2::read_xml(sprintf("%s/testdata.xml", exdatadir))
#'    root <- xml2::xml_root(parsed)
#'    vfmat <- xml_points24dash2(root, asvector = FALSE, dropXY = FALSE)
#'    plot_visual_field(vfmat)
#'    plot_visual_field(vfmat, "ACSDV")
#'    plot_visual_field(vfmat, "GDCSDV")
#'    plot_visual_field(vfmat, "ACSDPV", probs = TRUE)
#'    plot_visual_field(vfmat, "GDCSDPV", probs = TRUE)
#'
#'    par(mar = c(1,1,1,1), las = 1, pty = "s")
#'    plot_visual_field(vfmat, "GDCSDPV", probs = TRUE)
#'    plot_visual_field(vfmat, "GDCSDV", probs = FALSE, add = TRUE, textcolor = "red")
#'    abline(h = 0, v = 0)

plot_visual_field <- function(
    mat, valrow = "Sens Val",
    add = FALSE, probs = FALSE, textcolor = "black", strip.0 = TRUE,
    usedatarange = FALSE, addlegend = TRUE,
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
    graphics::axis(1, at = seq(-30, 30, 10), labels = FALSE, pos = 0)
    graphics::axis(2, at = seq(-30, 30, 10), labels = FALSE, pos = 0)
  }

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
