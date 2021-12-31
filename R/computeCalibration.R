#' Compute calibration
#'
#' @param labs Labels
#' @param preds Predictions
#' @param event_level Define event level
#' @param scale Scale calibration (default is FALSE)
#' @param plot Plot calibration (default is TRUE)
#'
#' @return Computed fraction of calibration
#' @export
computeCalibration <- function(labs, preds, event_level = "1", scale = F, plot = T) {
  # Scale predictions to 0-1 range
  if(scale) {
    min_pred <- min(preds)
    max_pred <- max(preds)
    preds <- (preds - min_pred) / (max_pred - min_pred)
    preds_cut <- cut(preds, 10)
  } else {
    preds_cut <- cut(preds, quantile(preds, prob = seq(0.00, 1.0, 0.1)), include.lowest = T)
  }
  # TODO: scale is not working

  df <- tibble(pred = preds,
               lab = labs) %>%
    mutate(preds_cut = cut(preds,
                           quantile(preds, prob = seq(0.00, 1.0, 0.1)),
                           include.lowest = T)) %>%
    arrange(preds_cut) %>%
    with_groups(preds_cut, ~ nest(.x))

  # Compute observed fraction and mean prediction
  df <- df %>%
    mutate(n = map_dbl(data, nrow),
           obs_frac = map_dbl(data, ~ sum(.x$lab == event_level) / nrow(.x)),
           mean_pred = map_dbl(data, ~ mean(.x$pred)))

  # Compute 95% CI
  df <- df %>%
    mutate(lci = pmax(0, (obs_frac - (1.96*(((obs_frac*(1-obs_frac))/n)^.5)))),
           uci = pmin(1, (obs_frac + (1.96*(((obs_frac*(1-obs_frac))/n)^.5)))))

  if(plot) {
    p1 <- ggplot(df, aes(x = mean_pred, y = obs_frac)) +
      geom_point() +
      geom_line() +
      geom_abline(aes(colour = "black", intercept = 0, slope = 1),
                  show.legend = T, linetype = 2, alpha = 0.5) +
      geom_smooth(method = "lm", fullrange = T, se = FALSE) +
      geom_errorbar(aes(ymin = lci, ymax = uci), colour="darkgrey", width=.02) +
      labs(x = "Mean prediction",
           y = "Fraction of positives") +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_bw() +
      theme(legend.position = "none") +
      coord_equal()

    p2 <- ggplot(df %>% unnest(data), aes(x = pred)) +
      geom_histogram(bins = 100) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      labs(x = "",
           y = "") +
      theme_bw() +
      theme(aspect.ratio = 0.1)

    layout = c(patchwork::area(t = 1, b = 10, l = 1, r = 10),
               patchwork::area(t = 11, b = 12, l = 1, r = 10))

    print(p2 / p1)
  }

  return(df)
}
