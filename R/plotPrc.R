#' Plot Precision Recall Curve
#'
#' @param x Tibble with precision and recall computed by the performance R package
#' @return A ggplot containing the precision recall curve
#' @export
plotPrc <- function(x) {
  x %>%
    ggplot2::ggplot(ggplot2::aes(x = recall, y = precision, colour = model)) +
    ggplot2::geom_path() +
    ggplot2::geom_abline(slope = -1, intercept = 1, color = "black", linetype = 2) +
    ggplot2::labs(x = "Recall (TPR, sensitivity)",
         y = "Precision (PPV)",
         colour = "Partition") +
    ggplot2::scale_x_continuous(breaks =  c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                       minor_breaks = c(seq(0, 0.1, by = 0.01), seq(0.9, 1, by = 0.01)),
                       limits = c(0, 1),
                       expand = c(0.005, 0.005)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                       minor_breaks = c(seq(0, 0.1, by = 0.01), seq(0.9, 1, by = 0.01)),
                       limits = c(0, 1),
                       expand = c(0.005, 0.005)) +
    ggplot2::coord_equal()
}
