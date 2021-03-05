# this test does not succeed when run interactively in RStudio
test_that("check warning in install_ibawds()", {
  expect_warning(install_ibawds(just_print = TRUE), "interactive use only")
})
