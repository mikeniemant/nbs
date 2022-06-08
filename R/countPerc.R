#' Count and percentage
#'
#' Simple function to count the frequencies of a variable (var) within a data frame (x) along with the percentage.
#' When a value (value) is provided, the function will return the number of defined observations and compute the percentage.
#'
#' @param x data fame
#' @param var variable
#' @param value value
#'
#' @return
#' @export
#'
countPerc <- function(x, var, value = NULL) {
  if(is.null(value)) {
    x %>%
      count(!!sym(var)) %>%
      mutate(perc = n/sum(n)*100)
  } else {
    x %>%
      count(!!sym(var) == value) %>%
      mutate(perc = n/sum(n)*100)
  }
}
