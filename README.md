
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NoBodySorrow package

<!-- badges: start -->
<!-- badges: end -->

NoBodySorrow (NBS) is an R package with a range of functions to improve
programming workflow in R.

## Installation

``` r
detach(package:nbs, unload=TRUE)
library(utils)
remove.packages("nbs")
devtools::install_github("mikeniemant/nbs")
```

# Functions

## cws

Clear work space - Remove all objects in environment - Close any SQL
connections - Close all clusters

## findTodo

Find all `#TODO` statements in all .R and .Rmd files in a a directory.

## replacePattern

Finds and replaces a particular pattern in all .R and .Rmd files in a
directory.

## themeDark2

Extension of ggplot2’s `theme_dark()` function to plot dark theme
visualisations.

# Notes

Open a new issue [here](https://github.com/mikeniemant/nbs/issues) for
any bug reports or feature requests.

Copyright (C) M.S.A. Niemantsverdriet, the Netherlands, 2019, all rights
reserved. Use for personal and educational purposes.
