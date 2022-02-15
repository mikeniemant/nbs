#' DLCV K-Nearest Neighbours No Hyperparameter Optimization
#'
#' @param folds rsample object with either group V-fold or the standard V-fold cross validation folds.
#' @param rec recipes recipe used for training
#' @return Tibble with k outer loop models, and training and testing predictions.
#' @export

dlcvKnnNoHp <- function(folds, rec) {
  knn_spec <- nearest_neighbor() %>%
    set_mode("classification") %>%
    set_engine("kknn")

  knn_workflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(knn_spec)

  knn_no_hp_folds <- folds %>%
    mutate(knn_model = map2(splits, id, ~ {
      set.seed(as.integer(stringr::str_sub(.y, nchar(.y), nchar(.y)))+1)
      knn_workflow %>% fit(data = analysis(.x))
    }),
    final_wf = map2(splits, knn_model, ~ {
      knn_workflow %>%
        finalize_workflow(.y) %>%
        fit(analysis(.x))
    }),
    map2_dfr(splits, final_wf, dlcvOuter))

  knn_no_hp_folds <- knn_no_hp_folds %>%
    select(-splits)

  return(knn_no_hp_folds)
}

#' DLCV K-Nearest Neighbours With Hyperparameter Optimization
#'
#' @param folds rsample object with either group V-fold or the standard V-fold cross validation folds.
#' @param rec recipes recipe used for training
#' @return Tibble with k outer loop models, and training and testing predictions.
#' @export
dlcvKnn <- function(folds, rec) {
  knn_spec <- nearest_neighbor(neighbors = tune(),
                               weight_func = tune(),
                               dist_power = tune()) %>%
    set_mode("classification") %>%
    set_engine("kknn")

  knn_workflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(knn_spec)

  knn_hp_grid <- grid_regular(neighbors(c(1,50)),
                              weight_func(),
                              dist_power(),
                              levels = 3)

  knn_folds <- folds %>%
    mutate(best_model = map2(splits, id, ~ {
      cat(paste0("KNN fold: ", stringr::str_sub(.y, nchar(.y), nchar(.y)), "\n"))
      dlcvInner(.x, knn_workflow, knn_hp_grid)
    })) %>%
    mutate(final_wf = map2(splits, best_model, ~ knn_workflow %>%
                             finalize_workflow(.y) %>%
                             fit(analysis(.x)))) %>%
    mutate(map2_dfr(splits, final_wf, dlcvOuter))

  knn_folds <- knn_folds %>%
    select(-splits)

  return(knn_folds)
}
