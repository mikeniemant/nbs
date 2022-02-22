#' DLCV Random Forest No Hyperparameter Optimization
#'
#' @param folds rsample object with either group V-fold or the standard V-fold cross validation folds.
#' @param rec recipes recipe used for training
#' @return Tibble with k outer loop models, and training and testing predictions.
#' @export

dlcvRfNoHp <- function(folds, rec) {
  rf_spec <- rand_forest() %>%
    set_mode("classification") %>%
    set_engine("randomForest")

  rf_workflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(rf_spec)

  rf_no_hp_folds <- folds %>%
    mutate(rf_model = map2(splits, id, ~ {
      set.seed(as.integer(stringr::str_sub(.y, nchar(.y), nchar(.y)))+1)
      rf_workflow %>% fit(data = analysis(.x))
    }),
    final_wf = map2(splits, rf_model, ~ {
      rf_workflow %>%
        finalize_workflow(.y) %>%
        fit(analysis(.x))
    }),
    map2_dfr(splits, final_wf, dlcvOuter))

  rf_no_hp_folds <- rf_no_hp_folds %>%
    select(-splits)

  return(rf_no_hp_folds)
}

#' DLCV Random Forest With Hyperparameter Optimization
#'
#' @param folds rsample object with either group V-fold or the standard V-fold cross validation folds.
#' @param rec recipes recipe used for training
#' @param rf_hp_grid Tuning grid dials objects with ranges for the following three random forest hyperparameters: mtry, min_n and trees
#' @return Tibble with k outer loop models, and training and testing predictions.
#' @export
dlcvRf <- function(folds, rec, rf_hp_grid = NULL) {
  rf_spec <- rand_forest(mtry = tune(),
                         min_n = tune(),
                         trees = tune()) %>%
    set_mode("classification") %>%
    set_engine("randomForest")

  rf_workflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(rf_spec)

  if(is.null(rf_hp_grid)) {
    rf_hp_grid <- grid_regular(trees(range = c(50, 350)),
                               mtry(range = c(3, 12)),
                               min_n(range = c(100, 300)),
                               levels = 3)
  }

  rf_folds <- folds %>%
    mutate(best_model = map(splits, ~ dlcvInner(.x, rf_workflow, rf_hp_grid))) %>%
    mutate(final_wf = map2(splits, best_model, ~ rf_workflow %>%
                             finalize_workflow(.y) %>%
                             fit(analysis(.x)))) %>%
    mutate(map2_dfr(splits, final_wf, dlcvOuter))

  rf_folds <- rf_folds %>%
    select(-splits)

  return(rf_folds)
}
