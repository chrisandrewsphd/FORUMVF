% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xml_dir_extract.R
\name{xml_dir_extract}
\alias{xml_dir_extract}
\title{Extract visual field values from all xml files in a directory}
\usage{
xml_dir_extract(
  xmldir,
  recursive = FALSE,
  csvdir = NULL,
  comment = FALSE,
  verbose = 0
)
}
\arguments{
\item{xmldir}{(character) Name of directory containing xml files from FORUM}

\item{recursive}{(logical) Should sub directories be included? Default is FALSE}

\item{csvdir}{(character) Name of directory to write csv file(s) to. Default (\code{NULL}) is not to write files.}

\item{comment}{(logical) Should text comment and patient comment be extracted? Default is FALSE.}

\item{verbose}{(integer) Amount of output to the console.  0 is none. Higher values may produce more output. Default 0.}
}
\value{
character matrix. One row per file.  First column is file name with complete path. Subsequent columns are extracted values from xml files.
}
\description{
Extract visual field values from all xml files in a directory
}
\examples{
# xml_dir_extract(".") # files in current directory
}
