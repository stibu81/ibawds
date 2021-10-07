test_that("check get_required_packages()", {
  req_pkgs <- get_required_packages()
  expect_type(req_pkgs, "character")
  pkgs <- c("dslabs", "stats", "grDevices", "methods", "rlang",
            "ggplot2", "scales", "dplyr", "stringr", "magrittr", "Ecdat",
            "kableExtra", "tidyverse", "rmarkdown", "caret", "reshape2",
            "lubridate", "ggrepel", "writexl", "cowplot", "DT", "gutenbergr",
            "tidytext", "rvest", "Lahman", "pdftools", "HistData", "titanic",
            "BiocManager", "waldo")
  expect_true(all(pkgs %in% req_pkgs))
  expect_false(any(c("R", "testthat", "usethis", "vdiffr", "covr") %in% req_pkgs))
})
