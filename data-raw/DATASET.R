generateDataset <- function(nobs = 100, nvar = 3, nreal = 2, seed = 42, name = NULL) {
  set.seed(seed)
  x <- matrix(rnorm(nobs * nvar), nobs, nvar)
  beta <- c(2+rnorm(nreal, 0, 1), rep(0, nvar - nreal))
  y_cont <- c(t(beta) %*% t(x)) + rnorm(nobs, sd = 3)
  y_logit <- exp(y_cont)/(1+exp(y_cont))
  y_class <- sapply(y_logit, function(x) rbinom(n = 1, size = 1, prob = x))

  if(is.null(name)) {
    dat <- dplyr::tibble(y = factor(y_class),
                         pred = y_logit)
  } else{
    dat <- dplyr::tibble(model = !!name,
                         y = factor(y_class),
                         pred = y_logit)
  }

  return(dat)
}

single_model_tibble <- generateDataset()

usethis::use_data(single_model_tibble, overwrite = TRUE)

multi_model_tibble <- dplyr::bind_rows(generateDataset(name = "model_1", seed = 1),
                                       generateDataset(name = "model_2", seed = 2))

usethis::use_data(multi_model_tibble, overwrite = TRUE)
