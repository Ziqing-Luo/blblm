
#' Split data into m parts of approximated equal sizes
#'
#' The subsamples may not in the same size.
#'
#' @param data A data.frame or [tibble][tibble::tibble-package] to be splited
#' @param m The number of subsamples.Integer.
#' @return a list of subsamples [tibble][tibble::tibble-package]
#' @export
#' @examples
#' split_data(mtcars, 3)
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>%
      purrr::set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (purrr::map(.x, .f, ...) %>% purrr::reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  purrr::map(.x, .f, ...) %>% purrr::reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  purrr::map(.x, .f, ...) %>% purrr::reduce(rbind)
}
