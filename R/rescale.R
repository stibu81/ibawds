#' Rescale Mean And/Or Standard Deviation of a Vector
#'
#' @param x numeric vector
#' @param mu numeric value giving the desired mean
#' @param sigma numeric value giving the desired standard
#'  deviation
#'
#' @examples
#' # create uniformely distributed random numers with
#' # fixed mean and standard deviation
#' x <- rescale(runif(1000, 5, 8))
#' mean(x)
#' sd(x)
#'
#' @export

rescale <- function(x, mu = 0, sigma = 1) {
  mu + sigma / stats::sd(x) * (x - mean(x))
}

