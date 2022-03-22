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
  # suppress messages that may be created when is_installed() loads packages
  if (suppressMessages(rlang::is_installed(required_packages))) {
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


#' Downgrade Packages to the Previous Version
#'
#' Downgrade packages to the previous version available on CRAN.
#' This is useful in order to prepare the system for a demonstration of
#' package updates.
#'
#' @param pkg character with the names of the packages to be downgraded.
#'
#' @details
#' Downgrading is only possible for packages that are currently installed. For
#' packages that are not installed, a warning is issued.
#'
#' The function uses [remotes::install_version()] to install a version
#' of a package that is older than the currently installed version.
#'
#' @return
#' A character vector with the names of the downgraded packages, invisibly.
#'
#' @export

downgrade_packages <- function(pkg) {

  vapply(pkg, downgrade_package, character(1)) %>%
    stats::na.omit() %>%
    as.vector() %>%
    invisible()
}

downgrade_package <- function(pkg) {

  if (!suppressMessages(rlang::is_installed(pkg))) {
    warning(pkg, " is not installed and cannot be downgraded.", call. = FALSE)
    return(NA_character_)
  }

  version <- as.character(utils::packageVersion(pkg))
  message("downgrading ", pkg, " from version ", version, " ...")
  remotes::install_version(pkg, version = paste("<", version))

  pkg
}
