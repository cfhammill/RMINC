% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/minc_interface.R
\name{mincApplyRCPP}
\alias{mincApplyRCPP}
\title{Perform Arbitrary calculations on a collection of mincVolumes}
\usage{
mincApplyRCPP(filenames, fun, ..., mask = NULL, maskval = NULL,
  filter_masked = FALSE, collate = identity)
}
\arguments{
\item{filenames}{The name of the files to apply over}

\item{fun}{the function to apply}

\item{...}{additional parameters to fun}

\item{mask}{a numeric mask vector}

\item{maskval}{An integer specifying the value inside the mask where to
apply the function. If left blank (the default) then anything
above 0.5 will be considered inside the mask. This argument
only works for mincApply, not pMincApply.}

\item{filter_masked}{Whether or not to remove the masked values
from the resultant object}

\item{collate}{A function to (potentially) collapse the result list
examples include link{unlist} and \link{simplify2array}, defaulting
to \link{identify} which returns the unaltered list.}
}
\value{
a list of results subject the the collate function
}
\description{
An RCPP variant of \link{mincApply}, the primary advantage being
that functions of an arbitrary number of arguments can be passed
to mincApplyRCPP.
}
