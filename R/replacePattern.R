#' Replace Pattern Function
#'
#' This function sets all paths
#'
#' Version 1.1 2019-11-15
#'
#' @param dir = directory
#' @param file.types = File types that should be assessed in dir
#' @param search.pattern = Search pattern to identify relevant files
#' @param pattern = Pattern to be replaced in all relevant file types
#' @param replacement = Replacement to replace files that match pattern
#' @export verbose = iteratively address the changes
#' @examples
#' replacePattern(dir = getwd(), file.type = "R", pattern = "cws")

replacePattern <- function(dir,
                           file.types = c("r", "R", "Rmd"),
                           search.pattern = NULL,
                           pattern,
                           replacement,
                           verbose = F) {
  # Define pattern search
  if(length(search.pattern) == 0) pattern <- paste0("(", paste(file.types, collapse = "|"), ")$", vebose = T)

  # List relevant documents
  rel.docs <- list.files(path = dir, recursive = T, pattern = search.pattern)
  
  # Define fileFindReplace function
  fileFindReplace <- function(filepath, pattern, replacement) {
    file_contents <- readLines(filepath)
    updated_contents <- gsub(x = file_contents, pattern = pattern, replacement = replacement)
    cat(updated_contents, file = filepath, sep = "\n")
  }
  
  # Define fileFindReplaceVerbose function
  fileFindReplaceVerbose <- function(filepath, pattern, replacement) {
    file_contents <- readLines(filepath)
    
    # Test parameters
    filepath <- "/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/R_packages/nbs_development_folder/cws.R"
    file_contents <- readLines(filepath)
    pattern <- "cat"
    replacement <- "bat"
    
    # Define empty line object
    pattern.lines <- NULL
    
    # Extract line indices that contain pattern
    for(i in 1:length(file_contents)) {
      if(grepl(pattern = pattern, x = file_contents[i])) pattern.lines <- c(pattern.lines, i)
    }
    
    if(length(pattern.lines) == 0) {
      print(paste0("No pattern instances were found in ", basename(filepath)))
    } else {
      print(paste0(length(pattern.lines), " pattern instances were found in ", basename(filepath)))
      
      verbose <- T
      # Address changes for each found pattern
      for(i in pattern.lines) {
        mock.line <- file_contents[(i-1):(i+1)]
        cor.line <- gsub(x = mock.line, pattern = pattern, replacement = replacement)
        #2.1 Show pattern (n number of lines)
        
        #2.2 Offer yes/no to replace pattern
        if(verbose) {
          cat(paste0("Replace line ", i, " in file ", basename(filepath)), "\n", 
              mock.line, "\n", 
              "with", "\n",
              cor.line, sep = "")
          output <- switch(menu(c("Replace", "Do not replace", "Complete script")) + 1,
                           cat("Nothing done\n"), 1, 2, 3)  
        }
        
        #2.3 Address replacement
        if(output == 1 | output == 3) {
          file_contents[i] <- gsub(x = file_contents[i], pattern = pattern, replacement = replacement)
          if (output == 3) {
            verbose = F
            output = 1
          }
        }
      }
      # Save updated file contents
      cat(file_contents, file = filepath, sep = "\n")
    }
  }

  if(verbose) {
    for(doc in rel.docs) {
      print(doc)
      dir.doc <- paste0(dir, doc)
      fileFindReplaceVerbose(dir.doc,
                             pattern,
                             replacement)
    }
  } else {
    # Replace pattern in each doc in rel.docs
    for(doc in rel.docs) {
      print(doc)
      dir.doc <- paste0(dir, doc)
      fileFindReplace(dir.doc,
                      pattern,
                      replacement)
    }
  }
}
