% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_visual_field.R
\name{plot_visual_field}
\alias{plot_visual_field}
\title{Plot an HVF based on extracted matrix or vector.}
\usage{
plot_visual_field(
  mat = NULL,
  valrow = "Sens Val",
  testpattern = c("XY", "24-2", "24-2C", "10-2", "30-2", "60-4"),
  imagelaterality = "match",
  addtoplot = FALSE,
  probs = FALSE,
  textcolor = "black",
  strip.0 = TRUE,
  usedatarange = FALSE,
  addlegend = TRUE,
  adddegrees = FALSE,
  eyestorageformat = "OD"
)
}
\arguments{
\item{mat}{A character matrix containing HVF test data, typically from \code{xml_extract_points(..., asvector = FALSE)}; a character vector, perhaps a subvector of \code{xml_extract_points(..., asvector = TRUE)}; or \code{NULL}, which will number the points of the specified \code{testpattern}: one of "24-2", "24-2C", "10-2", "30-2", and "60-4".}

\item{valrow}{If \code{mat} is a matrix, the name of the row in \code{mat} containing the values to plot. If \code{mat} is a vector, \code{valrow} is ignored.}

\item{testpattern}{Format of data provided. The default ("XY") will plot everything based on rows "X" and "Y" of \code{mat}. If X and Y are not part of \code{mat}, the coordinates must be inferred from the test pattern. Possibilities currently are "24-2", "24-2C", "10-2", "30-2", and "60-4".}

\item{imagelaterality}{Should the HVF be plotted as OD or OS? If \code{testpattern == "XY"}, the possible options are "match" (default), which uses the provided X and Y, or "flip", which flips the image horizontally across the vertical axis. If \code{testpattern != "XY"}, the possible options are "OD" and "OS" (or "match" (default), in which case \code{eyestorageformat} will be used, which is almost certainly "OD").}

\item{addtoplot}{Should the values be added to an existing plot (\code{TRUE}) or a new plot (\code{FALSE}, default)?}

\item{probs}{Are the values coded probabilities (\code{TRUE}) or VF scores (\code{FALSE}, default)?}

\item{textcolor}{Color of text to be plotted. Default is \code{"black"}.}

\item{strip.0}{Should trailing ".0" be removed before plotting? Default is \code{TRUE}.}

\item{usedatarange}{Should the graph boundaries be determined by rows "X" and "Y" of the data (\code{TRUE}) or default values for typical display for the specified test pattern (\code{FALSE}, default)?}

\item{addlegend}{Should the graph have a p-value legend at bottom. Default is \code{TRUE} (only relevant if \code{probs} is \code{TRUE}).}

\item{adddegrees}{Should the graph have the axes labeled with degrees? Default is \code{FALSE}.}

\item{eyestorageformat}{The default storage order in FORUMVF is OD, regardless of which eye is the subject of the test. Do not change this to "OS" except in very unusual circumstances. The format stores data generally from left to right, top to bottom. For the default "OD", this is Nasal to Temporal, Superior to Inferior.}
}
\value{
invisible NULL. Plot produced as side effect.
}
\description{
Plot an HVF based on extracted matrix or vector.
}
\examples{
   exdatadir <- system.file('extdata', package = 'FORUMVF')
   parsed <- xml2::read_xml(sprintf("\%s/testdata.xml", exdatadir))
   root <- xml2::xml_root(parsed)
   vfmat <- xml_points24dash2(root, asvector = FALSE, dropXY = FALSE)

   op <- par(mar = rep(1, 4))
   plot_visual_field(vfmat)
   plot_visual_field(vfmat, "ACSDV")
   plot_visual_field(vfmat, "GDCSDV")
   plot_visual_field(vfmat, "ACSDPV", probs = TRUE)
   plot_visual_field(vfmat, "GDCSDPV", probs = TRUE)

   par(mar = c(1,1,1,1), las = 1, pty = "s")
   plot_visual_field(vfmat, "GDCSDPV", probs = TRUE)
   plot_visual_field(vfmat, "GDCSDV", probs = FALSE, addtoplot = TRUE, textcolor = "red")
   abline(h = 0, v = 0)

   plot_visual_field(testpattern = "24-2")
   plot_visual_field(testpattern = "24-2", imagelaterality = "OS")

   plot_visual_field(testpattern = "24-2C", textcolor = "red", adddegrees = TRUE)
   plot_visual_field(testpattern = "24-2", textcolor = "black", addtoplot = TRUE)
   legend("topright", fill = c("black", "red"), c("24-2", "24-2C"), bty = "n")

   plot_visual_field(testpattern = "60-4", textcolor = "green", adddegrees = TRUE)
   plot_visual_field(testpattern = "30-2", textcolor = "blue", addtoplot = TRUE)
   plot_visual_field(testpattern = "10-2", textcolor = "purple", addtoplot = TRUE)
   legend("topright", fill = c("green", "blue", "purple"), c("60-4", "30-2", "10-2"), bty = "n")

   par(op)

}
