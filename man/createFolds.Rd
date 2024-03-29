% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dlcv_function.R
\name{createFolds}
\alias{createFolds}
\title{Create Folds}
\usage{
createFolds(x, k, group, strata, seed = 42)
}
\arguments{
\item{x}{data including outcome (strata) and group column}

\item{k}{number of folds}

\item{group}{group parameter}

\item{strata}{strata parameter}

\item{seed}{seed value}
}
\value{
A rsample object with folds
}
\description{
Create K number of grouped folds with a group and strata parameters
}
\details{
Version 1.0 2022-02-15
}
