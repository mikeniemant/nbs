#' Get Paths
#'
#' This function sets all paths
#'
#' Version 1.7 2020-01-07
#'
#' @param main.path Main path of the work environment
#' @keywords getPaths
#' @export
#' @param main.path = Main path of project
#' @param report.name = Report name
#' @param type = Type of document for a specific project directory
#' @param todo = Boolean to find all TODO: cases in the code directory
#' @param save.output = Boolean for saving the TODO output
#' @examples PATHS <- getPaths(main.path = "/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/000/4_data_science_in_practice/")
#' getPaths()

getPaths <- function(main.path, report.name = NA,
                     type = "report", todo = T, save.output = F) {
  if(substr(main.path, nchar(main.path), nchar(main.path)) != "/") {
    main.path <- paste0(main.path, "/")
  }

  path.list <- list(main = main.path,
                    code = paste0(main.path, "code/"),
                    data = paste0(main.path, "data/"),
                    `0_raw_data` = paste0(main.path, "data/0_raw/"),
                    `1_interim_data` = paste0(main.path, "data/1_interim/"),
                    `2_processed_data` = paste0(main.path, "data/2_processed/"),
                    `3_output_data` = paste0(main.path, "data/3_output/"),
                    figures = paste0(main.path, "figures/"),
                    reports = paste0(main.path, "reports/"),
                    results = paste0(main.path, "results/"),
                    modeling = paste0(main.path, "modeling/"))

  if(!is.na(report.name)) {
    if(substr(report.name, nchar(report.name), nchar(report.name)) != "/") {
      report.name <- paste0(report.name, "/")
    }

    if(type == "report") {
      path.list$report = paste0(path.list$reports, report.name)
      path.list$report_code = paste0(path.list$report, "code/")
      path.list$report_data = paste0(path.list$report, "data/")
      path.list$report_results = paste0(path.list$report, "results/")

    } else if(type == "modeling") {
      path.list$modeling_report = paste0(path.list$modeling, report.name)
      path.list$modeling_code = paste0(path.list$modeling_report, "code/")
      path.list$modeling_data = paste0(path.list$modeling_report, "data/")
      path.list$modeling_results = paste0(path.list$modeling_report, "results/")
    }
  }

  # find todo function
  if(todo) {
    # Define dir and search pattern for specific file types
    dir <- path.list$main

    search.pattern <- paste0("(", paste(c("r", "R", "Rmd", "rmd"), collapse = "|"),
                             ")$")

    # Identify related files
    rel.docs <- list.files(path = dir, recursive = T, pattern = search.pattern)

    if(save.output) {
      sink(file = paste0(path.list$main, "todo_output_", strftime(Sys.time(),"%Y-%m-%d_%H-%M"), ".txt"))
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

  if(save.output) {
   sink(NULL)
  }

  return(path.list)
}
