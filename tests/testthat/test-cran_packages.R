test_that("test n_available_packages() with valid input", {
  skip_on_cran()
  expect_equal(n_available_packages("2020-01-01"), 15368)
  expect_gte(n_available_packages(Sys.Date()),
             max(get_cran_history()$n_packages))
})

test_that("test n_available_packages() with invalid input", {
  expect_error(n_available_packages(10), "is not a valid date")
  expect_error(n_available_packages("2013-07-02"),
               "MRAN has no data for dates before 2014-09-17")
  expect_error(n_available_packages(Sys.Date() + 10),
               "MRAN has no data for dates in the future.")
  # setting these options causes file() to fail on URLs
  opts <- options(url.method = "none", encoding = "none")
  expect_error(n_available_packages(Sys.Date()),
               "Obtaining data from MRAN failed")
  options(opts)
})

test_that("test get_cran_history()", {
  expect_s3_class(get_cran_history(), "tbl_df")
  expect_gte(nrow(get_cran_history()), 57)
  expect_named(get_cran_history(), c("date", "n_packages"))
  expect_s3_class(get_cran_history()$date, "Date")
  expect_type(get_cran_history()$n_packages, "integer")
})