# CRAN URL must be set for the tests to work
options(repos = c(CRAN = "https://cloud.r-project.org"))

test_that("check get_required_packages()", {
  req_pkgs <- get_required_packages()
  expect_type(req_pkgs, "character")
  pkgs <- c("dslabs", "stats", "grDevices", "rlang",
            "ggplot2", "scales", "dplyr", "stringr", "magrittr", "deldir",
            "kableExtra", "tidyverse", "rmarkdown", "caret", "reshape2",
            "lubridate", "ggrepel", "writexl", "cowplot", "DT",
            "tidytext", "rvest", "Lahman", "HistData", "titanic",
            "BiocManager", "waldo", "clValid", "ggfortify",
            "knitr", "hexbin", "patchwork", "GGally", "party", "RANN", "ranger")
  expect_true(all(pkgs %in% req_pkgs))
  expect_false(any(c("R", "testthat", "usethis", "vdiffr", "covr") %in% req_pkgs))
})


# this test only succeeds on systems, where all the required
# packages are installed.
test_that("test install_ibawds()", {
  req_pkgs <- get_required_packages()
  skip_if_not(suppressMessages(rlang::is_installed(req_pkgs)),
              "not all the required packages are installed.")
  expect_message(install_ibawds(), "All the required packages are installed.")
})


# downgrade_packages() is only tested for a package that does not exist.
test_that("test downgrade_packages()", {
  expect_warning(
    expect_false(downgrade_packages("notoncran!")),
    "\"notoncran!\" is not installed and cannot be downgraded."
  )
})


test_that("test get_version_after_to_install()", {
  expect_equal(
    get_version_after_to_install(as.numeric_version("1.4.3"), "any"),
    as.numeric_version("1.4.3")
  )
  expect_equal(
    get_version_after_to_install(as.numeric_version("1.4.3.2"), "any"),
    as.numeric_version("1.4.3.2")
  )
  expect_equal(
    get_version_after_to_install(as.numeric_version("1.4.3"), "patch"),
    as.numeric_version("1.4.3")
  )
  expect_equal(
    get_version_after_to_install(as.numeric_version("1.4.3.2"), "patch"),
    as.numeric_version("1.4.3.0")
  )
  expect_equal(
    get_version_after_to_install(as.numeric_version("1.4.3"), "minor"),
    as.numeric_version("1.4.0")
  )
  expect_equal(
    get_version_after_to_install(as.numeric_version("1.4.3.2"), "minor"),
    as.numeric_version("1.4.0.0")
  )
  expect_equal(
    get_version_after_to_install(as.numeric_version("1.4.3"), "major"),
    as.numeric_version("1.0.0")
  )
  expect_equal(
    get_version_after_to_install(as.numeric_version("1.4.3.2"), "major"),
    as.numeric_version("1.0.0.0")
  )
})


test_that("test get_software_versions()", {
  sw <- get_software_versions()
  expect_named(sw, c("os", "R", "RStudio", "pkg_installed", "ibawds"))
  expect_named(sw$R, c("version", "date"))
  expect_named(sw$RStudio, c("version", "date"))
  expect_named(sw$ibawds, c("installed", "current"))
  expect_type(sw$os, "character")
  expect_s3_class(sw$R$version, "numeric_version")
  expect_s3_class(sw$R$date, "Date")
  # NA as version number is not supported for R < 4.4.0, so the check needs
  # to be different depending on the R version
  if (getRversion() >= "4.4.0") {
    expect_s3_class(sw$RStudio$version, "numeric_version")
  } else {
    expect_type(sw$RStudio$version, "character")
  }
  expect_s3_class(sw$RStudio$date, "Date")
  expect_true(sw$pkg_installed)
  expect_s3_class(sw$ibawds$installed, "numeric_version")
  expect_s3_class(sw$ibawds$current, "numeric_version")
})
