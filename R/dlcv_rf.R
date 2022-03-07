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
#' @param boruta Feature selection with Boruta
#' @return Tibble with k outer loop models, and training and testing predictions.
#' @export
dlcvRf <- function(folds, rec, rf_hp_grid = NULL, boruta = F) {
  rf_spec <- rand_forest(mtry = tune(),
                         min_n = tune(),
                         trees = tune()) %>%
    set_mode("classification") %>%
    set_engine("randomForest")

  if(is.null(rf_hp_grid)) {
    rf_hp_grid <- grid_regular(trees(range = c(50, 350)),
                               mtry(range = c(2, 6)),
                               min_n(range = c(100, 300)),
                               levels = 3)
  }

  if(boruta) {
    # Feature selection with Boruta
    bor_folds <- folds %>% dplyr::mutate(
      bor_imp = purrr::map(
        splits, ~
          {
            bor_fit <- Boruta::Boruta(x = rsample::analysis(.x) %>%
                                        dplyr::select(dplyr::all_of(features)), y = rsample::analysis(.x) %>%
                                        dplyr::pull(y))
            bor_imp <- dplyr::tibble(bor_fit$ImpHistory %>% dplyr::as_tibble() %>%
                                       tidyr::pivot_longer(cols = dplyr::everything(),
                                                           names_to = "var", values_to = "importance")) %>%
              dplyr::left_join(data.frame(var = names(bor_fit$finalDecision),
                                          decision = bor_fit$finalDecision, row.names = NULL),
                               by = "var")
            return(bor_imp)
          }
        ))

    # Extract predictors
    bor_folds <- bor_folds %>%
      dplyr::mutate(
        bor_predictors = purrr::map(bor_imp,
                                    ~.x %>% dplyr::filter(decision == "Confirmed") %>%
                                      dplyr::distinct(var) %>% dplyr::pull(var)))

    # Define formula for Boruta
    bor_folds <- bor_folds %>%
      mutate(form = map(bor_predictors, ~ as.formula(paste0("y ~ ", paste(.x, collapse = " + ")))))

    # Define workflow for RF DLCV
    rf_folds <- folds %>%
      bind_cols(bor_folds %>% select(bor_predictors)) %>%
      mutate(form = map(bor_predictors, ~ as.formula(paste0("y ~ ", group, " + ", paste(.x, collapse = " + "))))) %>%
      mutate(wf = map(form, ~ {
        rec <- recipe(.x,
                      data = x) %>%
          update_role(group, new_role = "id")

        rf_workflow <- workflow() %>% add_recipe(rec) %>% add_model(rf_spec)
        return(rf_workflow)
      }))

    # Perform DLCV
    rf_folds <- rf_folds %>%
      mutate(best_model = map2(splits, wf, ~ dlcvInner(.x, .y, rf_hp_grid)),
             final_wf = map2(wf, best_model,
                             ~ .x %>% finalize_workflow(.y)),
             final_wf = map2(splits, final_wf, ~ .y %>%
                               fit(analysis(.x)))) %>%
      mutate(map2_dfr(splits, final_wf, dlcvOuter))

    # Remove form and add the bor_predictors to best_model
    rf_folds <- rf_folds %>%
      mutate(best_model = map2(best_model, bor_predictors, ~ {
        .x <- .x %>% mutate(bor_predictors = list(.y))
      })) %>%
      select(-form, -wf, -bor_predictors)

  } else {
    rf_workflow <- workflow() %>% add_recipe(rec) %>% add_model(rf_spec)

    rf_folds <- folds %>%
      mutate(best_model = map(splits, ~ dlcvInner(.x, rf_workflow, rf_hp_grid))) %>%
      mutate(final_wf = map2(splits, best_model,
                             ~ rf_workflow %>% finalize_workflow(.y) %>%
                               fit(analysis(.x)))) %>%
      mutate(map2_dfr(splits,
                      final_wf, dlcvOuter))
  }

  rf_folds <- rf_folds %>%
    select(-splits)

  return(rf_folds)
}
