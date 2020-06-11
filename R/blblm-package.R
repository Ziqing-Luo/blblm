#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care
## usethis namespace: start
#' @importFrom tibble tibble
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' @import furrr
#'
