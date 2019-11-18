#' Write Data Into File
#'
#' This function creates an data description file
#'
#' Version 1.2 2019-11-15
#'
#' @param file.dir = file directory
#' @param file.name = file name
#' @param info.l = list of all data details; type(column) = info
#' @param file.type = type of file
#' @param info = add info column to output
#' @export
#' @examples
#' writeDateInfoFile()

writeDataInfoFile <- function(file.dir, file.name = "data_description", info.l, file.type = "txt", info = F) {
  # File path
  file.path <- paste0(file.dir, "/", file.name, ".", file.type)
  
  # Add info column
  if(info) {
    info.l <- append(info.l, list(info = ""))
  }
  
  # Define new line
  df <- dplyr::bind_rows(info.l)
  
  # How to fix the file type?
  if(file.type == "txt") {
    # Check if data description file exists
    if(file.exists(file.path)) {
      # Read txt file
      temp.df <- read.table(file.path, sep = ",", header = T)
      df <- rbind(temp.df, df)
    }
    
    # Create txt file
    write.table(x = temp.df, file = file.path, sep = ",", append = FALSE, 
                quote = F, row.names = FALSE)
    
  } else if(file.type == "xlsx") {
    if(file.exists(file.path)) {
      # Create txt file
      temp.df <- xlsx::read.xlsx(file.path, sheetIndex = "1", header = T)
      df <- rbind(temp.df, df)
    } 
    # Write output
    xlsx::write.xlsx(x = df, 
                     file = file.path,
                     sheetName = "1", 
                     row.names = F, 
                     col.names = T)
  } else {
    message("Please provide a file type")
    return(-1)
  }
}