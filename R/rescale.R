#' Rescale Mean And/Or Standard Deviation of a Vector
#'
#' @param x numeric vector
#' @param mu numeric value giving the desired mean
#' @param sigma numeric value giving the desired standard
#'  deviation
#'
#' @details
#' By default, mean and standard deviation are not changed, i.e.,
#' `rescale(x)` is identical to `x`. Only if a value is specified
#' for `mu` and/or `sigma` the mean and/or the standard deviation are
#' rescaled.
#'
#' @return
#' a numeric vector with the same length as `x` with mean `mu` and
#' standard deviation `sigma`.
#'
#' @examples
#' x <- runif(1000, 5, 8)
#'
#' # calling rescale without specifying mu and sigma doesn't change anything
#' all.equal(x, rescale(x))
#'
#' # change the mean without changing the standard deviation
#' x1 <- rescale(x, mu = 3)
#' all.equal(mean(x1), 3)
#' all.equal(sd(x1), sd(x))
#'
#' # rescale mean and standard deviation
#' x2 <- rescale(x, mu = 3, sigma = 2)
#' all.equal(mean(x2), 3)
#' all.equal(sd(x2), 2)
#'
#' @export

rescale <- function(x, mu = mean(x), sigma = sd(x)) {

  if (length(mu) > 1 || length(sigma) > 1) {
    warning("mu and sigma must be single numbers. Keeping only the first element.")
    mu <- mu[1]
    sigma <- sigma[1]
  }

  if (sigma < 0) {
    stop("sigma must be positive.")
  }

  mu + sigma / stats::sd(x) * (x - mean(x))
}

