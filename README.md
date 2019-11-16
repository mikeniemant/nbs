
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NoBodySorrow package

NoBodySorrow (NBS) is an R package with a range of functions to improve
programming workflow in R.

# Installation

``` r
detach(package:nbs, unload=TRUE)
library(utils)
remove.packages("nbs")
devtools::install_github("mikeniemant/nbs")
```

# Functions

## cws

Clear work space

  - Remove all objects in environment
  - Close any SQL connections
  - Close all clusters

## findLatestFile

Find latest file

  - Find the latest file with a particular file name pattern

## genHeadings

Generate headers

  - Print the headers of particular scripts
      - Normal R script
      - Function
      - R markdown file

## getPaths

Get paths

  - Creates an object with all paths to all folders within a project
    directory

## importPackages

Import packages; imports packages and, if not available, installs them.

## multiPlot

Multi plot allows you to plot multiple plots in one R device

# Notes

Open a new issue [here](https://github.com/mikeniemant/nbs/issues) for
any bug reports or feature requests.

Copyright (C) M.S.A. Niemantsverdriet, the Netherlands, 2019, all rights
reserved. Use for personal and educational purposes.
