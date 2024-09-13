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
    cli::cli_alert_success("All the required packages are installed.")
  } else {
    rlang::check_installed(required_packages)
  }
}


# nocov start

#' Check If the User Is Ready for the Course
#'
#' @description
#' Check if the current system is ready for the course by verifying the
#' following:
#'
#' * R and RStudio are up to date
#' * the ibawds package is up to date
#' * all the required packages are installed
#'
#' The function must be run from RStudio in order to run properly.
#'
#' @return
#' a logical indicating whether the system is up to date (invisibly).
#' Messages inform the user about the status of the system.
#'
#' @export

check_ibawds_setup <- function() {

  cli::cli_alert_info("Checking the setup for the course ...")
  cli::cli_alert_info(paste("Operating system:", utils::osVersion))

  ok <- TRUE

  # check the R version: it should be at most 1 year old
  r_date <- R.version.string %>%
    stringr::str_extract("(?<=\\()\\d{4}-\\d{2}-\\d{2}(?=\\))") %>%
    as.Date()
  r_ver <- R.version.string %>%
    stringr::str_extract("(?<=R version )\\d+\\.\\d+\\.\\d+")
  if (Sys.Date() - r_date < 365) {
    cli::cli_alert_success(paste("R is up to date:", r_ver))
  } else {
    cli::cli_alert_danger(paste("R is outdated:", r_ver))
    cli::cli_alert_info(
      c("Please install the current version from {.url https://cran.r-project.org/}")
    )
    ok <- FALSE
  }

  # check the RStudio version: it should be at most 1 year old
  rs_ver <- tryCatch(
    rstudioapi::versionInfo()$version,
    error = function(e) NA_character_
  )
  if (is.na(rs_ver)) {
    cli::cli_alert_danger("This function must be run from RStudio.")
    cli::cli_alert_info(
      c("If you don't have RStudio installed, please install it from ",
        "{.url https://posit.co/download/rstudio-desktop/}")
    )
    ok <- FALSE
  } else {
    rs_date <- as.Date(paste0(rs_ver[1, 1], "-", rs_ver[1, 2], "-01"))
    if (Sys.Date() - rs_date < 365) {
      cli::cli_alert_success(paste("RStudio is up to date:", rs_ver))
    } else {
      cli::cli_alert_danger(paste("RStudio is outdated:", rs_ver))
      cli::cli_alert_info(
        c("Please install the current version from ",
          "{.url https://posit.co/download/rstudio-desktop/}")
      )
      ok <- FALSE
    }
  }

  # check that all packages are installed
  required_packages <- get_required_packages()
  pkg_installed <- suppressMessages(rlang::is_installed(required_packages))
  if (!pkg_installed) {
    cli::cli_alert_danger("Some required packages are not installed.")
    cli::cli_alert_info(
      c("Please run {.run [install_ibawds()](ibawds::install_ibawds())} ",
        "to install them.")
    )
    ok <- FALSE
  } else {
    cli::cli_alert_success("All the required packages are installed.")
  }

  # check ibawds version: it should be the newest one
  ibawds_ver <- utils::packageVersion("ibawds")
  ibawds_cur <- utils::available.packages()["ibawds", "Version"]
  if (ibawds_ver < ibawds_cur) {
    cli::cli_alert_danger(paste("ibawds is outdated:", ibawds_ver))
    cli::cli_alert_info(
      c("Please run install.packages(\"ibawds\") to install the current version.")
    )
    ok <- FALSE
  } else {
    cli::cli_alert_success(paste("ibawds is up to date:", ibawds_ver))
  }

  # summarise
  if (ok) {
    emoji <- praise_emoji()
    cli::cli_alert_success(
      c(emoji, " Your system is ready for the course! ", emoji)
    )
  } else {
    cli::cli_alert_danger("Please fix the issues before starting the course.")
  }

  return(invisible(ok))

}


# this function is taken from the testthat package
# https://github.com/r-lib/testthat/tree/fe38519
# and is licensed under the MIT license

praise_emoji <- function() {
  if (!cli::is_utf8_output()) {
    return("")
  }

  emoji <- c(
    "\U0001f600", # smile
    "\U0001f973", # party face
    "\U0001f638", # cat grin
    "\U0001f308", # rainbow
    "\U0001f947", # gold medal
    "\U0001f389", # party popper
    "\U0001f38a" # confetti ball
  )
  sample(emoji, 1)
}

# nocov end


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

  vapply(pkg, downgrade_package, logical(1)) %>%
    stats::na.omit() %>%
    as.vector() %>%
    invisible()
}

downgrade_package <- function(pkg) {

  if (!suppressMessages(rlang::is_installed(pkg))) {
    cli::cli_warn(
      c(
        "!" = paste0("\"", pkg, "\" is not installed and cannot be downgraded.")
      )
    )
    return(FALSE)
  }

  version <- as.character(utils::packageVersion(pkg))
  cli::cli_alert_info(paste("downgrading", pkg, "from version", version, "..."))
  remotes::install_version(pkg, version = paste("<", version))

  TRUE
}

