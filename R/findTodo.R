#' Find todos
#'
#' Find all todo annotatations in project directory
#'
#' Version 1.0 2020-05-22
#'
#' @param dir Main path of project
#' @param save.output Boolean for saving the TODO output
#' @keywords findTodo
#' @export
findTodo <- function(dir, save.output = F) {
  search.pattern <- paste0("(", paste(c("r", "R", "Rmd", "rmd"), collapse = "|"),
                           ")$")

  # Identify related files
  rel.docs <- list.files(path = dir, recursive = T, pattern = search.pattern)

  if(save.output) {
    sink(file = paste0(dir, "todo_output_", strftime(Sys.time(),"%Y-%m-%d_%H-%M"), ".txt"))
  }

  # Iteratively search in all relevant files for TODO statement
  for (doc in rel.docs) {
    dir.doc <- paste0(dir, "/", doc)
    file_contents <- readLines(dir.doc)

    pattern.lines <- NULL
    for (i in 1:length(file_contents)) {
      if (grepl(pattern = "TODO:", x = file_contents[i], ignore.case = T))
        pattern.lines <- c(pattern.lines, i)
    }
    if (length(pattern.lines) != 0) {
      cat("-----------------------------------------------------------------",
          "\n")
      cat(paste0(length(pattern.lines), " TODO(s) found in ", paste0(basename(dir), "/", doc), "\n"))

      for (i in pattern.lines) {
        mock.line <- file_contents[(i - 1):(i + 1)]

        cat(
          paste0("TODO found in line ", i, " in file ",
                 basename(dir.doc)), "\n---\n",
          paste0(c(i-1, i, i+1), ": ", mock.line, sep = "\n"),
          "---\n",
          sep = "")
      }
    }
  }
}
