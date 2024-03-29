% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dlcv_boruta.R
\name{dlcvBoruta}
\alias{dlcvBoruta}
\title{DLCV Boruta}
\usage{
dlcvBoruta(folds, rec, features)
}
\arguments{
\item{folds}{rsample object with either group V-fold or the standard V-fold cross validation folds.}

\item{rec}{recipe object used for training}

\item{features}{features}
}
\value{
Tibble with k outer loop models, and training and testing predictions.
}
\description{
Double Loop Cross Validation with Boruta. Features are selected in the inner loop and used in the outer loop to train the model that is subsequently tested on the left-out fold in the outer loop.
}
