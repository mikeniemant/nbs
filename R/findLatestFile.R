#' Find latest file
#'
#' This function finds the latest file with a particular file name pattern
#' 
#' Version 1.2 2019-04-29
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
  if(is.na(file.name))
    warning("No file name found in dir '", directory, "' with pattern '", pattern, "'")
  file.dir <- paste0(directory, file.name)
  if(print)
    print(file.dir)
  return(file.dir)
}
