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
    cli::cli_warn(
      c("!" = paste("mu and sigma must be single numbers.",
                    "Keeping only the first element."))
    )
    mu <- mu[1]
    sigma <- sigma[1]
  }

  if (sigma < 0) {
    cli::cli_abort("sigma must be positive.")
  }

  mu + sigma / stats::sd(x) * (x - mean(x))
}


#' Create a Random Vector With Fixed Correlation With Another Vector
#'
#' `rand_with_cor()` creates a vector of random number that has
#' correlation `rho` with a given vector `y`.
#' Also mean and standard deviation of the random vector
#' can be fixed by the user. By default, they will be equal to the mean
#' and standard deviation of `y`, respectively.
#'
#' @param y a numeric vector
#' @param rho numeric value between -1 and 1 giving the desired correlation.
#' @inheritParams rescale
#'
#' @return
#' a vector of the same length as `y` that has correlation `rho` with `y`.
#'
#' @source
#' This solution is based on an
#' [answer](https://stats.stackexchange.com/a/313138/64220) by
#' [whuber](https://stats.stackexchange.com/users/919/whuber)
#' on [Cross Validated](https://stats.stackexchange.com).
#'
#' @examples
#' x <- runif(1000, 5, 8)
#'
#' # create a random vector with positive correlation
#' y1 <- rand_with_cor(x, 0.8)
#' all.equal(cor(x, y1), 0.8)
#'
#' # create a random vector with negative correlation
#' # and fixed mean and standard deviation
#' y2 <- rand_with_cor(x, -0.3, 2, 3)
#' all.equal(cor(x, y2), -0.3)
#' all.equal(mean(y2), 2)
#' all.equal(sd(y2), 3)
#'
#' @export

rand_with_cor <- function(y, rho, mu = mean(y), sigma = sd(y)) {

  if (abs(rho) > 1) {
    cli::cli_abort("rho must lie between -1 and 1.")
  }

  x <- stats::rnorm(length(y))
  y.perp <- stats::residuals(stats::lm(x ~ y))

  # x such that cor(x, y) = rho
  x <- rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)

  rescale(x, mu = mu, sigma = sigma)
}
