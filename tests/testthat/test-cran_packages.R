# CRAN URL must be set for the tests to work
options(repos = c(CRAN = "https://cloud.r-project.org"))

test_that("test n_available_packages()", {
  skip_on_cran()
  skip_on_ci()
  expect_type(n_available_packages(), "integer")
  expect_gt(n_available_packages(), 15000)
})

test_that("test available_r_version()", {
  skip_on_cran()
  skip_on_ci()
  expect_match(available_r_version(), "\\d\\.\\d\\.\\d")
})

test_that("tests with failing internet connection", {
  # setting these options causes file() to fail on URLs
  opts <- options(url.method = "none", encoding = "none")
  expect_error(n_available_packages(),
               "Obtaining data from CRAN failed")
  expect_error(available_r_version(),
               "Obtaining data from CRAN failed")
  options(opts)
})
