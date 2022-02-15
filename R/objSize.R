#' Object Size
#'
#' Compute memory size of objects in the R environment.
#'
#' Version 1.0 2022-02-15
#'
#' @param objs character vector with object name(s)
#' @return A tibble containing all objects arranged on object size (mb).
#' @export
objSize <- function(objs) {
  res <- data.frame(name = objs) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(obj_size_mb = purrr::map_dbl(name, ~ object.size(get(.x))/1000)) %>%
    dplyr::arrange(-obj_size_mb)

  return(res)
}
