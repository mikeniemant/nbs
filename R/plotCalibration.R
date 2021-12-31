#' Plot calibration
#'
#' @param x tibble with the computed mean_pred and obs_frac per model
#' @param plot_errorbar Plot error bar (default = TRUE)
#' @param plot_smooth Plot smooth lines (default = TRUE)
#' @param group Variable to group coordinates
#' @param title Plot title
#' @return A ggplot containing the calibration plot
#' @export
plotCalibration <- function(x, plot_errorbar = T, plot_smooth = T, group = NULL, title = NULL) {
  p <- ggplot2::ggplot(x, ggplot2::aes(x = mean_pred, y = obs_frac, colour = !!sym(group))) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_abline(ggplot2::aes(colour = "black", intercept = 0, slope = 1),
                         show.legend = T, linetype = 2, alpha = 0.5) +
    ggplot2::labs(x = "Mean prediction",
                  y = "Fraction of positives",
                  colour = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
                                group,
                                perl = TRUE),
                  title = title) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::coord_equal()

  if(plot_errorbar) p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lci, ymax = uci), colour = "darkgrey", width = .02)

  if(plot_smooth) p <- p +
      ggplot2::geom_smooth(method = "lm", fullrange = T, se = FALSE, alpha = 0.2)
  print(p)
}
