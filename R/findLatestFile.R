#' Find Latest Lile
#'
#' This function finds the latest file with a particular file name pattern
#' 
#' Version 1.3 2020-01-13
#' 
#' @param directory Directory of file
#' @param pattern String pattern of file
#' @param index Index number for selecting the i-latest file
#' @keywords findLatestFile
#' @export
#' @examples
#' findLatestFile()

findLatestFile <- function(directory, pattern, index = 1, print = F) {
  file.name <- tail(list.files(directory, pattern = pattern), index)[1]
  if(is.na(file.name)) {
    cat(paste0("No file name found in dir '", directory, "' with pattern '", pattern, "'", "\n"))
    cat(paste0("Files in ", directory, ": ", "\n"))
    cat(paste0(list.files(directory), collapse = "\n"))
    file.dir <- NA
  } else {
    file.dir <- paste0(directory, file.name)
    if(print)
      print(file.dir)
  }
  return(file.dir)  
}
