#' Create Folds
#'
#' Create K number of grouped folds with a group and strata parameters
#'
#' Version 1.0 2022-02-15
#'
#' @param x data including outcome (strata) and group column
#' @param k number of folds
#' @param group group parameter
#' @param strata strata parameter
#' @param seed seed value
#' @return A rsample object with folds
#' @export
createFolds <- function(x, k, group, strata, seed = 42) {
  set.seed(seed)

  # Create folds
  folds <- group_vfold_cv(data = x, group = !!group, v = k, strata = !!strata)

  # Compute label prevalence in each group
  lab_prev <- map_dbl(folds$splits, ~ round(sum(as.data.frame(.x)[, strata] == "1")/nrow(.x)*100, 2))
  ggplot(tibble(lab_prev), aes(x = "label prevalence", y = lab_prev)) +
    geom_boxplot() +
    ggbeeswarm::geom_beeswarm()
  summary(lab_prev)

  # Check if first target level is event
  lab_notation <- map_chr(folds$splits, ~ levels(as.data.frame(x)[, strata])[1])
  if(all(lab_notation == "1")) {
    cat(paste0("Outcome \"", unique(lab_notation), "\" is correctly the first level in all folds\n"))
  } else {
    cat("Error")
  }

  # Check if study IDs have been grouped correctly
  lab_overlap <- map_chr(folds$splits, ~ {
    studyid_overlap <- intersect(unique(analysis(.x)[, group, drop = T]),
                                 unique(assessment(.x)[, group, drop = T]))
    return(if_else(length(studyid_overlap) == 0, "No overlap", "Overlap"))
  })

  if(all(lab_overlap == "No overlap")) {
    cat(paste0("No overlap in \"", group, "\" parameter\n"))
  } else {
    cat("Error")
  }
  return(folds)
}

#' Double Loop Cross Validation (DLCV) Outer
#'
#' Functionality to perform the DLCV outer loop.
#'
#' Version 1.0 2022-02-15
#'
#' @param x_outer Configuration of the outer loop folds
#' @param tuned_model Tuned model
#'
#' @return Tibble with all computed predictions for the outer train and training folds.
#' @export
dlcvOuter <- function(x_outer, tuned_model) {
  trn_preds <- predict(tuned_model, analysis(x_outer), type = "prob")$.pred_1
  tst_preds <- predict(tuned_model, assessment(x_outer), type = "prob")$.pred_1

  output <- tibble(part = c(rep("train", times = nrow(analysis(x_outer))),
                            rep("test", times = nrow(assessment(x_outer)))),
                   y = c(analysis(x_outer)$y, assessment(x_outer)$y),
                   .pred_1 = c(trn_preds, tst_preds))

  if(length(ls(pattern = "f_")) > 0) {
    output <- output %>%
      mutate(!!f_1 := c(analysis(x_outer)[, f_1, drop = T], assessment(x_outer)[, f_1, drop = T]),
             !!f_2 := c(analysis(x_outer)[, f_2, drop = T], assessment(x_outer)[, f_2, drop = T]),
             !!f_3 := c(analysis(x_outer)[, f_3, drop = T], assessment(x_outer)[, f_3, drop = T]))
  }

  output_n <- output %>%
    with_groups(part, ~ .x %>% nest()) %>%
    pivot_wider(names_from = "part", values_from = "data") %>%
    rename(trn_preds = train,
           tst_preds = test)

  return(output_n)
}

#' Double Loop Cross Validation (DLCV) Inner
#'
#' Functionality to perform the DLCV inner loop.
#'
#' Version 1.0 2022-02-15
#'
#' @param x_inner Configuration of the inner loop folds
#' @param wf Workflow
#' @param hp_grid Hyperparameter grid
#'
#' @return Tibble with all computed predictions for the inner train and training folds.
#' @export
dlcvInner <- function(x_inner, wf, hp_grid) {
  inner_folds <- vfold_cv(x_inner %>% analysis(), v = 10, repeats = 1, strata = y)

  # Perform hp optimization
  model_tune <- tune_grid(wf,
                          resamples = inner_folds,
                          grid = hp_grid)

  show_best(model_tune, metric = "roc_auc") %>%
    select(.config, mean)

  return(select_best(model_tune, metric = "roc_auc") %>%
           left_join(show_best(model_tune, metric = "roc_auc") %>%
                       select(.config, mean),
                     by = ".config"))
}

#' Compute Cross Validation Area Under the Curve
#'
#' Estimate the area under the curve (AUC) metric from computed cross validation metrics.
#'
#' @param folds Nested tibble object with the training and test predictions, trn_preds and tst_preds respectively.
#' @param model Model
#' @param confidence Confidence
#' @return Tibble with the computed AUC with CI for both training and test folds.
#' @export
computeCvAUC <- function(folds, model, confidence = 0.95) {
  .x <- folds %>% unnest(trn_preds) %>% select(id, y, .pred_1)
  trn_cv_auc <- cvAUC::ci.cvAUC(predictions = .x$.pred_1,
                                labels = .x$y,
                                folds = .x$id,
                                confidence = confidence)

  .x <- folds %>% unnest(tst_preds) %>% select(id, y, .pred_1)
  tst_cv_auc <- cvAUC::ci.cvAUC(predictions = .x$.pred_1,
                                labels = .x$y,
                                folds = .x$id,
                                confidence = confidence)

  res <- bind_rows(tibble(model,
                          part = "train",
                          mean_auc = trn_cv_auc$cvAUC,
                          ci_lower = trn_cv_auc$ci[1],
                          ci_upper = trn_cv_auc$ci[2],
                          confidence = trn_cv_auc$confidence),
                   tibble(model,
                          part = "test",
                          mean_auc = tst_cv_auc$cvAUC,
                          ci_lower = tst_cv_auc$ci[1],
                          ci_upper = tst_cv_auc$ci[2],
                          confidence = tst_cv_auc$confidence))

  return(res)
}

#' Plot Discriminative Metrics
#'
#' Functionality to plot both ROC and PRC curves for both the training and tests folds.
#'
#' Version 1.0 2022-02-15
#'
#' @param x Nested tibble object
#' @param bl Discriminative metrics for a baseline model (sensitivity, specificity and PPV)
#' @param group Group parameter
#' @param title Plot title
#' @param legend Boolean to visualise legend
#' @param breaks Boolean to plot additional breaks and lines
#' @return Cowplot visualisation with four plots; ROC and PRC curves for both the training and tests folds.
#' @export
plotDisMet <- function(x, bl = NA, group = "id", title = "", legend = F, breaks = F) {
  p1 <- x %>%
    mutate(roc = map(trn_preds, ~ roc_curve(.x, y, .pred_1))) %>%
    select(!!group, roc) %>%
    unnest(roc) %>%
    plotRoc(group = group, title = paste(title, "ROC train folds"), breaks = breaks) +
    theme(legend.position = "none")

  p2 <- x %>%
    mutate(roc = map(tst_preds, ~ roc_curve(.x, y, .pred_1))) %>%
    select(!!group, roc) %>%
    unnest(roc) %>%
    plotRoc(group = group, title = paste(title, "ROC test folds"), breaks = breaks) +
    theme(legend.position = "none")

  p3 <- x %>%
    mutate(prc = map(trn_preds, ~ pr_curve(.x, y, .pred_1))) %>%
    select(!!group, prc) %>%
    unnest(prc) %>%
    plotPrc(group = group, title = paste(title, "PRC train folds"), breaks = breaks) +
    theme(legend.position = "none")

  p4 <- x %>%
    mutate(prc = map(tst_preds, ~ pr_curve(.x, y, .pred_1))) %>%
    select(!!group, prc) %>%
    unnest(prc) %>%
    plotPrc(group = group, title = paste(title, "PRC test folds"), breaks = breaks) +
    theme(legend.position = "none")

  if(all(!is.na(bl))) {
    p1 <- p1 +
      geom_point(data = bl, aes(x = 1 - spec, y = sens, colour = NULL))
    p2 <- p2 +
      geom_point(data = bl, aes(x = 1 - spec, y = sens, colour = NULL))
    p3 <- p3 +
      geom_point(data = bl, aes(x = sens, y = ppv, colour = NULL))
    p4 <- p4 +
      geom_point(data = bl, aes(x = sens, y = ppv, colour = NULL))
  }

  p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)

  if(legend) {
    legend <- cowplot::get_legend(
      p1 +
        guides(color = guide_legend()) + # nrow = 3
        theme(legend.position = "right") # bottom
    )

    # bottom
    # p <- cowplot::plot_grid(p, legend, ncol = 1, rel_heights = c(1, .1))
    # right
    p <- cowplot::plot_grid(p, legend, ncol = 2, rel_widths = c(0.6, 0.1))
  }

  return(p)
}
