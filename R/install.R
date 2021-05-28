#' Install the R-Packages Required for the Course
#'
#' A number of R-packages are used in the courses and
#' the video lectures. They are also dependencies of
#' this package. Use `install_ibawds()` to install the
#' packages that are not yet installed.
#'
#' @param just_print logical. If `TRUE`, the function will just print a
#'  message with the packages that need to be installed (if any) and stops
#'  without installing them.
#'
#' @details
#' This function checks whether all the packages that `ibawds` depends on,
#' imports or suggests are installed. A message informs the user about missing
#' packages and asks, whether they should be installed. If the process is
#' aborted, no packages are installed.
#'
#' @return
#' Invisible logical indicating whether package installation was successful.
#' `TRUE` is returned also when all required packages were already installed.
#'
#' @export

install_ibawds <- function(just_print = FALSE) {

  if (!interactive()) {
    warning("This function is intended for interactive use only.")
    return(invisible(FALSE))
  }

  # extract all dependencies of ibawds from DESCRIPTION
  required_packages <- system.file("DESCRIPTION", package = "ibawds") %>%
    read.dcf() %>%
    magrittr::extract(, c("Depends", "Imports", "Suggests")) %>%
    stringr::str_split(",") %>%
    unlist() %>%
    # remove version numbers. Use dotall = TRUE because the condition for the
    # package version sometimes contains a line break (e.g., "(>=\n2.1.0)").
    stringr::str_remove(stringr::regex("\\(.*\\)", dotall = TRUE)) %>%
    stringr::str_trim() %>%
    # remove the entries for R, testthat and usethis
    setdiff(c("R", "testthat", "usethis"))
  is_installed <- check_installed(required_packages)

  success <- FALSE
  if (all(is_installed)) {
    message("All required packages are installed.")
    success <- TRUE
  } else {
    to_install <- required_packages[!is_installed]
    message("Some required packages are missing.")
    message("The following package(s) will be installed: ", paste(to_install, collapse = ", "), ".")
    if (just_print) return(invisible(FALSE))
    ans <- utils::menu(c("Yes", "No"), title = "Do you want to continue?")
    if (ans == 1) {
      utils::install.packages(to_install)
    } else {
      return(invisible(FALSE))
    }

    # check success of installation
    is_installed2 <- check_installed(required_packages)
    if (all(is_installed2)) {
      message("Packages were installed successfully")
      success <- TRUE
    } else {
      failed <- required_packages[!is_installed2]
      message("The installation of the following packages failed: ",
              paste(failed, collapse = ", "), ".")
    }
  }

  invisible(success)
}


# check whether a package is installed
check_installed <- function(pkgs) {

  paths <- suppressWarnings(find.package(pkgs))

  vapply(pkgs,
         function(pkg) any(stringr::str_detect(paths, pkg)),
         logical(1))
}
