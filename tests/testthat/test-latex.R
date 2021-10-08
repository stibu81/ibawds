test_that("test define_latex_stats()", {
  expect_output(out <-define_latex_stats(), "\\\\newcommand\\{")
  expect_null(out)
})
