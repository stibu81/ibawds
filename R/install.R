#' Install the R-Packages Required for the Course
#'
#' @export

install_ibawds <- function() {

  required_packages <- c("tidyverse", "dslabs", "rmarkdown", "caret",
                         "reshape2", "lubridate", "WriteXLS", "ggrepel",
                         "writexl")
  is_installed <- check_installed(required_packages)

  success <- FALSE
  if (all(is_installed)) {
    message("All required packages are installed.")
    success <- TRUE
  } else {
    to_install <- required_packages[!is_installed]
    message("Some required packages are missing.")
    message("Installing ", paste(to_install, collapse = ", "), ".")
    utils::install.packages(to_install)

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
