#' Install the R-Packages Required for the Course
#'
#' A number of R-packages are used in the courses and
#' the video lectures. They are also dependencies of
#' this package. Use `install_ibawds()` to install the
#' packages that are not yet installed.
#'
#' @details
#' This function checks whether all the packages that `ibawds` depends on,
#' imports or suggests are installed. In interactive sessions, it either
#' informs the user that all packages are installed or asks to install
#' missing packages. The function relies on [rlang::check_installed()].
#'
#' @return
#' nothing or `NULL` invisibly
#'
#' @export

install_ibawds <- function() {

  # extract all dependencies of ibawds from DESCRIPTION
  required_packages <- get_required_packages()

  # if all packages are installed, inform the user and exit
  # otherwise, ask to install the missing packages
  if (rlang::is_installed(required_packages)) {
    message("All the required packages are installed.")
  } else {
    rlang::check_installed(required_packages)
  }
}


get_required_packages <- function() {

  system.file("DESCRIPTION", package = "ibawds") %>%
    read.dcf() %>%
    magrittr::extract(, c("Depends", "Imports", "Suggests")) %>%
    stringr::str_split(",") %>%
    unlist() %>%
    # remove version numbers. Use dotall = TRUE because the condition for the
    # package version sometimes contains a line break (e.g., "(>=\n2.1.0)").
    stringr::str_remove(stringr::regex("\\(.*\\)", dotall = TRUE)) %>%
    stringr::str_trim() %>%
    # remove the entries for R and packages that are not relevant for the students
    setdiff(c("R", "testthat", "usethis", "vdiffr", "covr"))

}
