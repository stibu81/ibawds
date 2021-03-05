test_that("test rescaling of vectors", {
  set.seed(1234)
  x <- runif(100)
  expect_equal(rescale(x), x)
  expect_equal(mean(rescale(x, mu = 12)), 12)
  expect_equal(sd(rescale(x, mu = 12)), sd(x))
  expect_equal(sd(rescale(x, sigma = 7)), 7)
  expect_equal(mean(rescale(x, sigma = 7)), mean(x))
  expect_equal(mean(rescale(x, mu = 3, sigma = 7)), 3)
  expect_equal(sd(rescale(x, mu = 3, sigma = 7)), 7)
})

test_that("test rescale() with bad inputs", {
  expect_warning(rescale(1:10, mu = 1:2), "must be single numbers")
  expect_warning(rescale(1:10, sigma = 1:2), "must be single numbers")
  expect_error(rescale(1:10, sigma = -5), "sigma must be positive")
})
