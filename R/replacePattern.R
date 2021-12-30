#' Replace Pattern
#'
#' This function searches documents for matches to the argument pattern and
#' replaces in with a new pattern
#'
#' Version 1.2 2019-11-19
#'
#' @param dir = directory
#' @param file.types = File types that should be assessed in dir
#' @param file.pattern = Search pattern to identify relevant files
#' @param pattern = Pattern to be replaced in all relevant file types
#' @param replacement = Replacement to replace files that match pattern
#' @rawNamespace importFrom("utils", "menu")
# Dummy code to test function
# cd ~
# mkdir ~/test_replace_pattern \\
# echo "test pattern cat" >> ~/test_replace_pattern/test_1.R \\
# echo "test pattern Cat" >> ~/test_replace_pattern/test_2.R \\
replacePattern <- function(dir,
                           file.types = c("r", "R", "Rmd"),
                           file.pattern = NULL, # Pattern for file.type searches
                           pattern,
                           replacement) {
  # Add slash to dir
  if(substr(dir, nchar(dir), nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }

  if(length(file.pattern) == 0) {
    file.pattern <- paste0("(", paste(file.types, collapse = "|"),
                             ")$")
  }
  rel.docs <- list.files(path = dir, recursive = T, pattern = file.pattern)
  fileFindReplace <- function(filepath, pattern, replacement) {
    file_contents <- readLines(filepath)
    updated_contents <- gsub(x = file_contents, pattern = pattern,
                             replacement = replacement)
    cat(updated_contents, file = filepath, sep = "\n")
  }
  fileFindReplaceVerbose <- function(filepath, pattern, replacement) {
    file_contents <- readLines(filepath)

    pattern.lines <- NULL
    for(i in 1:length(file_contents)) {
      if(grepl(pattern = pattern, x = file_contents[i]))
        pattern.lines <- c(pattern.lines, i)
    }
    if(length(pattern.lines) == 0) {
      print(paste0("No pattern instances were found in ",
                   basename(filepath)))
    }
    else {
      print(paste0(length(pattern.lines), " pattern instances were found in ",
                   basename(filepath)))
      verbose <- T
      for(i in pattern.lines) {
        mock.line <- file_contents[(i - 1):(i + 1)]
        cor.line <- gsub(x = mock.line, pattern = pattern,
                         replacement = replacement)
        if(verbose) {
          cat(paste0("Replace line ", i, " in file ",
                     basename(filepath)), "\n\n", mock.line,
              "\n", "-->", "\n", cor.line, "\n",
              sep = "")
          output <- switch(menu(c("Replace", "Do not replace",
                                  "Complete script", "Stop")) + 1, cat("Nothing done\n"),
                           1, 2, 3, 4)
        }
        if(output == 1 | output == 3) {
          file_contents[i] <- gsub(x = file_contents[i],
                                   pattern = pattern, replacement = replacement)
          if(output == 3) {
            verbose = F
            output = 1
          }
        }
        if(output == 4) {
          return(-1)
        }
      }
      cat(file_contents, file = filepath, sep = "\n")
    }
  }

  for(doc in rel.docs) {
    print(doc)
    dir.doc <- paste0(dir, doc)
    if(fileFindReplaceVerbose(dir.doc, pattern, replacement) == -1) {
      message("Function stopped")
      break()
    }
  }
}
