library(dplyr)

N_train <- 1000
N_test <- 20
x_max <- 1.2

set.seed(1143)

poly10 <- function(x, sigma = 1) {
  2 * x - 10 * x^5 + 15 * x^ 10 + rnorm(length(x), sd = sigma)
}

noisy_data <- list(
  train = tibble(
    x = seq(0.05, 0.95, length.out = N_train),
    y = poly10(x)
  ),
  test = tibble(
    x = runif(N_test, min = 0, max = x_max),
    y = poly10(x)
  )
)

usethis::use_data(noisy_data, overwrite = TRUE)
