#' Install the R-Packages Required for the Course
#'
#' A number of R-packages are used in the courses and
#' the video lectures. They are also dependencies of
#' this package. Use `install_ibawds()` to install the
#' packages that are not yet installed.
#'
#' @export

install_ibawds <- function() {

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
    stringr::str_trim()
  is_installed <- check_installed(required_packages)

  success <- FALSE
  if (all(is_installed)) {
    message("All required packages are installed.")
    success <- TRUE
  } else {
    to_install <- required_packages[!is_installed]
    message("Some required packages are missing.")
    message("The following packages will be installed: ", paste(to_install, collapse = ", "), ".")
    ans <- utils::menu(c("Yes", "No"), title = "Do you want to continue?")
    if (ans == 1) {
      utils::install.packages(to_install)
    } else {
      return(invisible(FALSE))
    }

    # check success of installation
    is_installed2 <- check_installed(required_packages)
    if (all(is_installed2)) {
      message("Packages wer installed successfully")
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
