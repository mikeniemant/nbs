#' Summarize Function
#'
#' @param x Data (Data frame / Tibble)
#' @param colname Name of column of interest (character)
#' @param check_normality Check normality (Boolean)
#' @param normality Normality, if known (Boolean)
#' @param print_plot Print plot (Boolean)
#'
#' @return
#' @export
#'
summarizeFunc <- function(x, colname, check_normality = T, normality = T, print_plot = F) {
  if(check_normality) {
    # Check for normality with shapiro test
    # if < 0.05, significantly different from normal distribution, no normality
    shap_test <- shapiro.test(x[, colname, drop = T])$p.value
    pracma::fprintf("Shapiro-Wilk normality test: %f\n", round(shap_test, 3))
    if(shap_test < 0.05) {
      normality <- F
    }
  }

  if(print_plot) {
    # p <- ggplot(x, aes(sample = !!sym(colname))) +
    #   qqplotr::stat_qq_band(alpha = 0.5) +
    #   geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
    #   qqplotr::stat_qq_line(alpha = 0.5) +
    #   qqplotr::stat_qq_point(alpha = 0.5) +
    #   labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

    dp <- list(mean = 2, sd = 2)
    p <- ggplot(x, aes(sample = !!sym(colname))) +
      qqplotr::stat_pp_band(dparams = dp) +
      qqplotr::stat_pp_line() +
      qqplotr::stat_pp_point(dparams = dp) +
      labs(x = "Probability Points", y = "Cumulative Probability")
    print(p)
  }

  if(normality) {
    x %>%
      summarize(n = n(),
                n_na = sum(is.na(!!sym(colname))),
                min = min(!!sym(colname), na.rm = T),
                iq1 = quantile(!!sym(colname), probs = 0.25, na.rm = T, names = F),
                median = median(!!sym(colname), na.rm = T),
                iq3 = quantile(!!sym(colname), probs = 0.75, na.rm = T, names = F),
                max = max(!!sym(colname), na.rm = T))
  } else {
    x %>%
      summarize(n = n(),
                n_na = sum(is.na(!!sym(colname))),
                min = min(!!sym(colname), na.rm = T),
                iq1 = quantile(!!sym(colname), probs = 0.25, na.rm = T, names = F),
                median = median(!!sym(colname), na.rm = T),
                iq3 = quantile(!!sym(colname), probs = 0.75, na.rm = T, names = F),
                max = max(!!sym(colname), na.rm = T))
  }
}
