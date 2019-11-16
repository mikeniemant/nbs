#' Get Paths Function
#'
#' This function sets all paths
#'
#' Version 1.5 2019-11-04
#'
#' @param main.path Main path of the work environment
#' @keywords getPaths
#' @export
#' @examples
#' getPaths()

getPaths <- function(main.path, report.name = NA, type = "report") {
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

  return(path.list)
}
