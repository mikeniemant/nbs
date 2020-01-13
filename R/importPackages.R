#' Import Packages
#'
#' This function imports packages and, if not available, installs them.
#' 
#' Version 1.3 2019-05-14
#' 
#' @param packages list of required packages
#' @param silent load packages in silent mode
#' @keywords importPackages
#' @export
#' @examples installAndLoadPackages("ggplot2", "caret", "RCurl")
#' importPackages()

importPackages <- function(..., silent = T) {
  packages <- list(...)
  
  # Check if packages are installed
  new.pkg <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # Install missing packages
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  
  # Load all packages
  for (pkg in packages) {
    if(silent) {
      suppressPackageStartupMessages(require(pkg, character.only = T, warn.conflicts = F, quietly = T))
    } else {
      library(pkg)
    }
  }
}
