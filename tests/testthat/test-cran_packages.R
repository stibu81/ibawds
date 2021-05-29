test_that("test n_available_packages()", {
  # don't test an actual call to MRAN because this failes on win-builder
  # and (presumable) on CRAN
  # expect_equal(n_available_packages("2020-01-01"), 15368)
  expect_error(n_available_packages(10), "is not a valid date")
  expect_error(n_available_packages("2013-07-02"),
               "MRAN has no data for dates before 2014-09-17")
  expect_error(n_available_packages(Sys.Date() + 10),
               "MRAN has no data for dates in the future.")
})

test_that("test get_cran_history()", {
  expect_s3_class(get_cran_history(), "tbl_df")
  expect_lte(nrow(get_cran_history()), 56)
  expect_named(get_cran_history(), c("date", "n_packages"))
  expect_s3_class(get_cran_history()$date, "Date")
  expect_type(get_cran_history()$n_packages, "integer")
})
