#' DLCV XGBoost No Hyperparameter Optimization
#'
#' @param folds rsample object with either group V-fold or the standard V-fold cross validation folds.
#' @param rec recipes recipe used for training
#' @return Tibble with k outer loop models, and training and testing predictions.
#' @export

# dlcvXgbNoHp <- function(folds, rec) {
#   xgb_spec <- rand_forest() %>%
#     set_mode("classification") %>%
#     set_engine("randomForest")
#
#   xgb_workflow <- workflow() %>%
#     add_recipe(rec) %>%
#     add_model(xgb_spec)
#
#   xgb_no_hp_folds <- folds %>%
#     mutate(xgb_model = map2(splits, id, ~ {
#       set.seed(as.integer(stringr::str_sub(.y, nchar(.y), nchar(.y)))+1)
#       xgb_workflow %>% fit(data = analysis(.x))
#     }),
#     final_wf = map2(splits, xgb_model, ~ {
#       xgb_workflow %>%
#         finalize_workflow(.y) %>%
#         fit(analysis(.x))
#     }),
#     map2_dfr(splits, final_wf, dlcvOuter))
#
#   xgb_no_hp_folds <- xgb_no_hp_folds %>%
#     select(-splits)
#
#   return(xgb_no_hp_folds)
# }

#' DLCV XGBoost With Hyperparameter Optimization
#'
#' @param folds rsample object with either group V-fold or the standard V-fold cross validation folds.
#' @param rec recipes recipe used for training
#' @return Tibble with k outer loop models, and training and testing predictions.
#' @export
# dlcvXgb <- function(folds, rec) {
#   xgb_spec <- rand_forest(mtry = tune(),
#                          min_n = tune(),
#                          trees = tune()) %>%
#     set_mode("classification") %>%
#     set_engine("randomForest")
#
#   xgb_workflow <- workflow() %>%
#     add_recipe(rec) %>%
#     add_model(xgb_spec)
#
#   # xgb_hp_grid <- grid_regular(trees(range = c(50, 350)),
#   #                            mtry(range = c(3, 12)),
#   #                            min_n(range = c(100, 300)),
#   #                            levels = 3)
#
#   xgb_folds <- folds %>%
#     mutate(best_model = map(splits, ~ dlcvInner(.x, xgb_workflow, xgb_hp_grid))) %>%
#     mutate(final_wf = map2(splits, best_model, ~ xgb_workflow %>%
#                              finalize_workflow(.y) %>%
#                              fit(analysis(.x)))) %>%
#     mutate(map2_dfr(splits, final_wf, dlcvOuter))
#
#   xgb_folds <- xgb_folds %>%
#     select(-splits)
#
#   return(xgb_folds)
# }
