
#' Linear Regression with Little Bag of Bootstraps
#'
#' @param formula mpg ~ wt * hp.see [formula][stats::formula]
#' @param data All data.
#' @param m The number of subsamples.Integer.
#' @param B Times of bootstrap
#'
#' @rdname blblm
#' @return list of coef and sigma
#' @export
#' @examples
#' fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' coef(fit)
#' confint(fit, c("wt", "hp"))
#'

blblm <- function(formula, data, m = 10, B = 5000) {
  data_list <-  split_data(mtcars, m)
  n <-  nrow(mtcars)

  estimates <- data_list %>%
    furrr::future_map(~ lm_subsample_boot(formula , df = .x, n, B))

  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' @param file_paths The path to csv/tsv files, which are subsamples of the dataset.
#' @export
#' @rdname blblm
#'
#' @examples
#'
#' \dontrun{
#' file_pahts = list.files(pattern = "*.csv")
#' blblm_files(mpg ~ wt * hp, file_pahts, B = 100)
#' }
#'
blblm_files <- function(formula, file_paths, B = 5000) {
  stopifnot(all(file.exists(file_paths)))

  # total size of all data
  n <- nrow(vroom::vroom(file_paths, col_types = vroom::cols()))

  estimates <- file_paths %>% furrr::future_map( ~ {
    # read each file as subsample
    df <- vroom::vroom(.x, col_types = vroom::cols())
    lm_subsample_boot(formula, df, n, B)
  }, .progress = TRUE)

  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' Subsample Parallel lm bootstrap
#'

#' @param df data.frame from a split subsample
#' @param n The nrow of all data
#'
#' @rdname blblm
#' @export
#' @examples
#' n =  nrow(mtcars)
#' data_list = split_data(mtcars, 3)
#' lm_subsample_boot(mpg ~ wt * hp,data_list[[1]], n,100)

lm_subsample_boot <- function(formula, df, n, B) {
  purrr::map(seq_len(B), ~ {
    freqs <- rmultinom(1, n, rep(1, nrow(df)))

    # drop the original closure of formula,
    # otherwise the formula will pick a wront variable from the global scope.
    environment(formula) <- environment()

    fit <- lm(formula, data = df, weights = freqs)

    p <- fit$rank
    y <- model.extract(fit$model, "response")
    e <- fitted(fit) - y
    w <- fit$weights
    sigma = sqrt(sum(w * (e ^ 2)) / (sum(w) - p))

    list(coef = coef(fit), sigma = sigma)
  })
}



# # compute the coefficients from fit
# blbcoef <- function(fit) {
#   coef(fit)
# }
#
# # compute sigma from fit
# blbsigma <- function(fit) {
#   p <- fit$rank
#   y <- model.extract(fit$model, "response")
#   e <- fitted(fit) - y
#   w <- fit$weights
#   sqrt(sum(w * (e^2)) / (sum(w) - p))
# }


#' New print way

#' @param x \code{blblm} object
#' @param ...
#'
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}

#' Calculate the sigma confidence level

#' @param object \code{blblm} object
#'
#' @param confidence Boolean, TRUE OR FALSE.
#' @param level Confidence level, default 0.95.
#' @param ...
#'
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(purrr::map_dbl(est, ~ mean(purrr::map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(purrr::map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      purrr::set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' Calculate the estimates coef
#'
#' @param object \code{blblm} object
#'
#' @param ...
#'
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans(na.rm = TRUE))
}

#' Calculate the confidence level of independent variables


#' @param parm variables that are used to fit the blblm model.
#' @inheritParams sigma.blblm
#'
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(fit$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}



#'Predict the the new data use fitted blblm model.
#'
#' @param new_data  New data.frame of varables
#' @inheritParams sigma.blblm
#'
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans(na.rm = TRUE))
  }
}
