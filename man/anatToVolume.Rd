% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/minc_anatomy.R
\name{anatToVolume}
\alias{anatToVolume}
\title{Converts a column from an anatLm model into a volume for viewing or saving}
\usage{
anatToVolume(anat, labelVolume, column, defs = attr(anat, "definitions"))
}
\arguments{
\item{anat}{The anatModel}

\item{labelVolume}{The volume containing the label definitions}

\item{column}{String indicating which column to turn into a volume}

\item{defs}{The path to the label definitions}
}
\value{
The volume with the values from the anatModel
}
\description{
Converts a column from an anatLm model into a volume for viewing or saving
}
\examples{
\dontrun{
labelVol <- mincArray(mincGetVolume("some-labels.mnc"))
alm <- anatLm(~Sex, gfBasic, vols)
statsvol <- anatToVolume(hLm, labelVol, "F.statistic")
mincPlotSliceSeries(anatVol, statsvol, anatLow = 700, anatHigh = 1400, 
  low=1, high=10, symmetric = F, begin=50, end=-50)
}
}
