#' DLCV Support Vector Machine No Hyperparameter Optimization
#'
#' @param folds rsample object with either group V-fold or the standard V-fold cross validation folds.
#' @param rec recipes recipe used for training
#' @return Tibble with k outer loop models, and training and testing predictions.
#' @export

dlcvSvmNoHp <- function(folds, rec) {
  svm_spec <- svm_poly() %>%
    set_mode("classification") %>%
    set_engine("kernlab", scaled = F) # TODO: what does this mean

  svm_workflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(svm_spec)

  svm_no_hp_folds <- folds %>%
    mutate(svm_model = map2(splits, id, ~ {
      set.seed(as.integer(stringr::str_sub(.y, nchar(.y), nchar(.y)))+1)
      svm_workflow %>% fit(data = analysis(.x))
    }),
    final_wf = map2(splits, svm_model, ~ {
      svm_workflow %>%
        finalize_workflow(.y) %>%
        fit(analysis(.x))
    }),
    map2_dfr(splits, final_wf, dlcvOuter))

  svm_no_hp_folds <- svm_no_hp_folds %>%
    select(-splits)

  return(svm_no_hp_folds)
}

#' DLCV Support Vector Machine Hyperparameter Optimization
#'
#' @param folds rsample object with either group V-fold or the standard V-fold cross validation folds.
#' @param rec recipes recipe used for training
#' @return Tibble with k outer loop models, and training and testing predictions.
#' @export
dlcvSvm <- function(folds, rec) {

  svm_spec <- svm_poly(cost = tune()) %>%
    set_mode("classification") %>%
    set_engine("kernlab", scaled = F) # TODO: what does this mean

  svm_workflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(svm_spec)

  svm_hp_grid <- grid_regular(cost(),
                              levels = 3)

  svm_folds <- folds %>%
    mutate(best_model = map(splits, ~ dlcvInner(.x, svm_workflow, svm_hp_grid))) %>%
    mutate(final_wf = map2(splits, best_model, ~ svm_workflow %>%
                             finalize_workflow(.y) %>%
                             fit(analysis(.x)))) %>%
    mutate(map2_dfr(splits, final_wf, dlcvOuter))

  svm_folds <- svm_folds %>%
    select(-splits)

  return(svm_folds)
}
