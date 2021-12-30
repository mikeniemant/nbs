#' Compute base net benefit
#'
#' @param y outcome as integer
#' @param xstart start threshold
#' @param xstop stop threshold
#' @param step step threshold
#' @param type type of net benefit
#' @return A data frame with the computed base net benefit
#' @export
computeBaseNetBenefit <- function(y, xstart = 0.01, xstop = 0.99, step = 0.01, type = "treated") {
  # Compute event rate
  event_rate <- mean(y == "1")

  # Define result output
  # - probability threshold range
  # - net benefit all
  # - net benefit none
  res <- dplyr::tibble(pt = seq(from = xstart, to = xstop, by = step),
                       nball = event_rate-(1-event_rate)*pt/(1-pt),
                       nbnone = 1-event_rate-event_rate*(1-pt)/pt)

  # Depending on type, multiple nball and nbnone by coefficient
  res <- res %>%
    dplyr::mutate(nball = dplyr::case_when(type == "treated" | type == "overall" ~ nball * 1,
                                           type == "untreated" ~ nball * 0,
                                           type == "adapt" ~ nball * (1-pt)),
                  nbnone = dplyr::case_when(type == "treated" ~ nbnone * 0,
                                            type == "untreated" | type == "overall" ~ nbnone * 1,
                                            type == "adapt" ~ nbnone * pt))

  return(res)
}
