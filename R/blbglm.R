#' Generalized Linear Regression with Little Bag of Bootstraps
#'
#' If \code{family = gaussian()}, \code{blbglm} is the same as \code{blblm}.
#'
#'
#' @param formula mpg ~ wt * hp.see [formula][stats::formula]
#' @param family  \code{stats::binomial()}, see [family][stats::family]
#' @param data All data.
#' @param m The number of subsamples.Integer.
#' @param B Times of bootstrap
#'
#' @rdname blbglm
#' @return list of coef and sigma
#' @export
#' @examples
#' \dontrun{
#' fit <- blbglm(vs ~ mpg+hp, family = binomial(),data = mtcars, m = 3, B = 100)
#' coef(fit)
#' confint(fit, c("wt", "hp"))
#' }
blbglm <- function(formula, family, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  n <- nrow(data)

  estimates <- data_list %>%
    furrr::future_map(~ glm_subsample_boot(formula, family, df = .x, n, B))

  res <- list(estimates = estimates, formula = formula)
  class(res) <- c("blblm","blbglm")
  invisible(res)
}


#' @param file_paths The path to csv/tsv files, which are subsamples of the dataset.
#' @export
#' @rdname blbglm
#'
#' @examples
#'
#' \dontrun{
#' file_pahts <- list.files(pattern = "*.csv")
#' blblm_files(vs ~ mpg+hp, family = binomial(),file_pahts, B = 100)
#' }
blbglm_files <- function(formula, file_paths, B = 5000) {
  stopifnot(all(file.exists(file_paths)))

  # total size of all data
  n <- nrow(vroom::vroom(file_paths, col_types = vroom::cols()))

  estimates <- file_paths %>% furrr::future_map(~ {
    # read each file as subsample
    df <- vroom::vroom(.x, col_types = vroom::cols())
    glm_subsample_boot(formula, family, df = .x, n, B)
  }, .progress = TRUE)

  res <- list(estimates = estimates, formula = formula)
  class(res) <- c("blblm","blbglm")
  invisible(res)
}


#' Subsample Parallel glm bootstrap
#'

#' @param df data.frame from a split subsample
#' @param n The nrow of all data
#'
#' @rdname blbglm
#' @export
#' @examples
#' n <- nrow(mtcars)
#' data_list <- split_data(mtcars, 3)
#' glm_subsample_boot(vs ~ mpg+hp, family = binomial(), data_list[[1]], n, 100)
glm_subsample_boot <- function(formula, family, df, n, B) {
  purrr::map(seq_len(B), ~ {
    freqs <- rmultinom(1, n, rep(1, nrow(df)))

    # drop the original closure of formula,
    # otherwise the formula will pick a wront variable from the global scope.
    environment(formula) <- environment()
    environment(family) <- environment()
    fit <- glm(formula,family = family, data = df, weights = freqs)

    # how to calculate glm sigma?
    # p <- fit$rank
    # y <- model.extract(fit$model, "response")
    # y <- as.numeric(y)
    # e <- as.numeric(fitted(fit)) - y
    # w <- fit$weights
    # sigma <- sqrt(sum(w * (e^2)) / (sum(w) - p))
    sigma = NULL

    list(coef = coef(fit), sigma = sigma)
  })
}



