#' Compute net benefit
#'
#' @param pt probability threshold values
#' @param y outcome as integer
#' @param pred predictions (probabilities)
#' @param type type of benefit plot
#' @return A data frame with the copmuted net benefit
#' @export
computeNetBenefit <- function(pt, y, pred, type = "treated") {
  res <- tibble(pt = pt) %>%
    dplyr::mutate(metrics = purrr::map(pt, ~ tibble(tp = sum(pred >= .x & y == 1),
                                                    fp = sum(pred >= .x & y == 0),
                                                    fn = sum(pred < .x & y == 1),
                                                    tn = sum(pred < .x & y == 0)))) %>%
    dplyr::mutate(nbt = purrr::map2_dbl(pt, metrics, ~ .y$tp / sum(.y) - .y$fp / sum(.y) * (.x/(1-.x))),
                  nbu = purrr::map2_dbl(pt, metrics, ~ .y$tn / sum(.y) - .y$fn / sum(.y) * ((1-.x)/.x)))

  if(type == "treated") {
    return(res %>% dplyr::select(pt, nbt))
  }
}
