test_that("check warning in install_ibawds()", {
  # don't run this test interactively
  if (!interactive()) {
    expect_warning(install_ibawds(just_print = TRUE), "interactive use only")
  }
})
