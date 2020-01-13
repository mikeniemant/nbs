#' Generate Headings
#' 
#' This function prints out the head for a script
#' Version 1.9 2019-10-29
#' 
#' @param type Type of heading (script, function)
#' @param title Title of section
#' @keywords genHeadings
#' @export
#' @examples
#' genHeadings()

generateHeaders <- function(type = "script", title = "", work.email = T) {
  if(work.email) {
    email.s = "m.niemantsverdriet@skylinedx.com"
  } else {
    email.s = "michael_niemantsverdriet@hotmail.com"
  }
  
  if(type == "script") {
    cat("####", "\n",
        "#Filename : ", title, "\n",
        "#Project  : ", "\n",
        "#Version  : 0.1", "\n",
        "#Author   : Michael S.A. Niemantsverdriet", "\n",
        "#Date     : ", as.character(Sys.Date()), "\n",
        "#Email    : ", email.s, "\n",
        "#Links    : -", "\n",
        "#Remarks  : -", "\n",
        "####", "\n",
        "\n",
        "# Prepare work environment ----", "\n",
        "library(nbs)", "\n",
        "library(tidyverse)", "\n",
        "\n",
        "# Import data ----", "\n",
        "PATHS <- getPaths()", "\n",
        "setwd(dirname(rstudioapi::getActiveDocumentContext()$path))",
        sep = "")
  } else if(type == "function") {
    cat("####", "\n",
        "#Function: ", title, "\n",
        "#Computations: \n",
        "#Input: ", "\n",
        "#Output: \n",
        "#Remarks: \n",
        "####",
        sep = "")
  } else if(type == "rmd") {
    cat("---", "\n",
        'title: "', title, '"', " \n",
        'author: "', "Michael Niemantsverdriet", '"', "\n",
        'date: "', "`r format(Sys.time(), '%d %B, %Y')`", '"', "\n",
        "output:", "\n",
        '  html_document:', '\n',
        '    df_print: paged', '\n',
        '    keep_md: true', '\n',
        '    toc_depth: 3', '\n',
        '    toc_float: false', '\n',
        '    toc: true', '\n',
        '    #fig_width: 6', '\n', 
        '    #fig_height: 8', '\n',
        '    #theme: darkly', '\n', 
        '    #highlight: zenburn #espresso or zenburn', '\n',
        'editor_options: ', '\n',
        '  chunk_output_type: console', '\n',
        '---', '\n\n',
        '```{r setup, include = F}', '\n',
        'knitr::opts_chunk$set(cache = T)', '\n',
        '```', '\n',
        
        '# Prepare work environment', '\n',
        '```{r pwe cache = F}', '\n',
        ' # Import packages and function', '\n',
        'library(nbs)', '\n',
        'importPackages("tidyverse", "captioner", "kableExtra", silent = T)', '\n', '\n',
        
        '# Define paths', '\n',
        'PATHS <- getPaths("")', '\n',
        '```', sep = '')
  }
}
