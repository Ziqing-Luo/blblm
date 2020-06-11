#' Set the paralle options.
#'
#'
#' @param n Number of parallel workers.
#'
#' @return NULL
#'
#' @examples
#'
#' \dontrun{
#' # require(furrr)
#' set_workers(4)
#' }
#'
#' @export
set_workers <- function(n = 2L) {
  # stopifnot(is.integer(n)) # make sure n is integer
  # stopifnot(n <= available_workers())
  future::plan(future::multiprocess, workers = n)
}

#' @export
available_workers <- function() {
  wks <- future::availableWorkers()
  c(system = length(wks))
}
