% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dlcv_regularization.R
\name{fipL1}
\alias{fipL1}
\title{Feature importance L1 DLCV}
\usage{
fipL1(x, plot = T, min_n = 0, y_nudge = NULL)
}
\arguments{
\item{x}{DLCV l1 object generated by the dlcvL1 function}

\item{plot}{Boolean to visualise plot}

\item{min_n}{Minimal number of folds to visualise}

\item{y_nudge}{Nudge y coordinate to visualise number of times features was selected in fold.}
}
\value{
Tibble with the feature importances and number of times selected in folds.
}
\description{
Visualize feature importance of the L1 DLCV results
}
