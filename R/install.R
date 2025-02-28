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
  sw <- get_software_versions()
  ok <- TRUE

  cli::cli_alert_info(paste("Operating system:", sw$os))

  # check the R version: it should be at most 1 year old
  if (Sys.Date() - sw$R$date < 365) {
    cli::cli_alert_success(paste("R is up to date:", sw$R$version))
  } else {
    cli::cli_alert_danger(paste("R is outdated:", sw$R$version))
    cli::cli_alert_info(
      c("Please install the current version from {.url https://cran.r-project.org/}")
    )
    ok <- FALSE
  }

  # check the RStudio version: it should be at most 1 year old
  if (is.na(sw$RStudio$version)) {
    cli::cli_alert_danger("This function must be run from RStudio.")
    cli::cli_alert_info(
      c("If you don't have RStudio installed, please install it from ",
        "{.url https://posit.co/download/rstudio-desktop/}")
    )
    ok <- FALSE
  } else if (Sys.Date() - sw$RStudio$date < 365) {
    cli::cli_alert_success(paste("RStudio is up to date:", sw$RStudio$version))
  } else {
    cli::cli_alert_danger(paste("RStudio is outdated:", sw$RStudio$version))
    cli::cli_alert_info(
      c("Please install the current version from ",
        "{.url https://posit.co/download/rstudio-desktop/}")
    )
    ok <- FALSE
  }

  # check that all packages are installed
  if (!sw$pkg_installed) {
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
  if (sw$ibawds$installed < sw$ibawds$current) {
    cli::cli_alert_danger(paste("ibawds is outdated:", sw$ibawds$installed))
    cli::cli_alert_info(
      c("Please run install.packages(\"ibawds\") to install the current version.")
    )
    ok <- FALSE
  } else {
    cli::cli_alert_success(paste("ibawds is up to date:", sw$ibawds$installed))
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


# determine software versions for check_ibawds_setup()
# this is done in a separate function such that it can be tested
get_software_versions <- function() {

  os <- utils::osVersion

  # get R version and release date
  R <- list(
    version = getRversion(),
    date = paste(R.version$year, R.version$month, R.version$day, sep = "-") %>%
      as.Date()
  )


  # get RStudio version and release date
  RStudio <- list(
    version = tryCatch(
        rstudioapi::versionInfo()$version,
        error = function(e) {
          # NA as version number is not supported for R < 4.4.0
          if (getRversion() >= "4.4.0") {
            as.numeric_version(NA_character_)
          } else {
            NA_character_
          }
        }
      ),
    date = as.Date(NA_character_)
  )
  if (!is.na(RStudio$version)) {
    RStudio$date <- paste0(
        RStudio$version[1, 1], "-", RStudio$version[1, 2], "-01"
      ) %>%
      as.Date()
  }

  # check that all packages are installed
  suppressMessages(
    pkg_installed <- get_required_packages() %>%
      rlang::is_installed()
  )

  # get ibawds version (installed and current)
  ibawds <- list(
    installed = utils::packageVersion("ibawds"),
    current = utils::available.packages()["ibawds", "Version"] %>%
    as.numeric_version()
  )

  list(
    os = os,
    R = R,
    RStudio = RStudio,
    pkg_installed = pkg_installed,
    ibawds = ibawds
  )

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
    setdiff(c("R", "testthat", "usethis", "vdiffr", "covr", "spelling",
              "withr", "httr2"))

}


#' Downgrade Packages to an Older Version
#'
#' Downgrade packages to an older version available on CRAN. This can be useful
#' when debugging problems that might have arisen due to a package update.
#'
#' @param pkg character with the names of the packages to be downgraded.
#' @param dec_version character giving the version to decrease. Possible
#' values are "any", "patch", "minor", and "major". See 'Details'.
#'
#' @details
#' Using the argument `dec_version`, the user can control which version will
#' be installed. The possible values are:
#'
#' \describe{
#' \item{`"any"`}{The previous available version will be installed.}
#' \item{`"patch"`}{The newest available version with a smaller patch version
#'   number will be installed. For packages with three version numbers, this
#'   is the same as using `"any"`.}
#' \item{`"minor"`}{The newest available version with a smaller minor version
#'   number will be installed.}
#' \item{`"major"`}{The newest available version with a smaller major version
#'   number will be installed.}
#' }
#'
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

downgrade_packages <- function(pkg,
                               dec_version = c("any", "patch", "minor", "major")) {

  dec_version <- match.arg(dec_version)

  vapply(pkg, downgrade_package, logical(1), dec_version) %>%
    stats::na.omit() %>%
    as.vector() %>%
    invisible()
}

downgrade_package <- function(pkg, dec_version) {

  if (!suppressMessages(rlang::is_installed(pkg))) {
    cli::cli_warn(
      c(
        "!" = paste0("\"", pkg, "\" is not installed and cannot be downgraded.")
      )
    )
    return(FALSE)
  }

  version <- utils::packageVersion(pkg)
  cli::cli_alert_info(paste0("installed version of ", pkg, ": ", version))
  version_after <- get_version_after_to_install(version, dec_version)
  cli::cli_alert_info(paste("downgrade to version before", version_after, "..."))
  remotes::install_version(pkg, version = paste("<", version_after))
  cli::cli_alert_info(
    paste("installed", pkg, "version", utils::packageVersion(pkg))
  )

  TRUE
}


# determine the version *after* the one to be installed. This is done by
# setting all the version numbers to zero after the one to be decreased.
# E.g., if dec_version = "minor", from 1.5.3.2, we would get 1.5.0.0.
get_version_after_to_install <- function(version, dec_version) {

  if (dec_version == "any") return(version)

  # set all version numbers after the one to be decreased to zero
  n_ver_num <- length(unclass(version)[[1]])
  i_dec_ver <- which(c("major", "minor", "patch") == dec_version)
  if (n_ver_num > i_dec_ver) {
    version[1, (i_dec_ver + 1):n_ver_num] <- 0
  }

  version
}


# nocov start

#' Find Packages Used For Lectures not Installed by ibawds
#'
#' ibawds offers the function [install_ibawds()] which installs all the packages
#' that are required for the course. `check_lecture_packages()` finds all the
#' packages that are used in the slides and exercise solution inside a directory.
#' It then checks whether they are all installed by `install_ibawds()` and
#' returns a tibble of those that are not. This can help to identify, if
#' additional packages need to be installed by `install_ibawds()`.
#'
#' @param path the path to a folder inside the directory with the slides and
#'  exercise solutions. The function automatically tries to identify the
#'  top level directory of the course material.
#'
#' @return
#' a tibble with two columns:
#'
#' \describe{
#'   \item{file}{the file where the package is used}
#'   \item{package}{the name of the package}
#' }
#'
#' @export


check_lecture_packages <- function(path = ".") {

  path <- find_lectures_root(path)

  # we must catch all the slides (*.Rmd, *.qmd) and the exercises
  # (*.Rmd, *.qmd or *.R) but avoid caches (*.RData)
  r_files <- path %>%
    list.files("\\.(R|q)($|md$)", recursive = TRUE, full.names = TRUE) %>%
    stringr::str_remove(paste0(path, "/?")) %>%
    # keep only files that belong to lectures. These folders always start with two
    # digits
    stringr::str_subset("^\\d{2}_[^/]*/")

  used_packages <- lapply(r_files, get_used_packages, path = path) %>%
    dplyr::bind_rows() %>%
    tidyr::separate_wider_delim("file",
                                delim = "/",
                                names = c("lecture", "type", "file"),
                                too_many = "merge")

  used_packages %>%
    dplyr::filter(!.data$package %in% get_ibawds_packages()) %>%
    dplyr::arrange(.data$package, .data$lecture, .data$type, .data$file)

}


# Get the names of all the packages that install_ibawds() will install, i.e.,
# those directly installed and their dependencies
get_ibawds_packages <- function() {
  get_required_packages() %>%
    tools::package_dependencies() %>%
    unlist() %>%
    c(get_required_packages(), "ibawds") %>%
    unique()
}

# nocov end


# find the root directory of the lectures. It is the directory that contains
# the folder "01_R_Basics".
find_lectures_root <- function(path, error_call = rlang::caller_env()) {

  if (!dir.exists(path)) {
    cli::cli_abort("Directory \"{path}\" does not exist.",
                   call = error_call)
  }

  root_cand <- normalizePath(path, mustWork = TRUE)

  is_root <- \(dir) dir.exists(file.path(dir, "01_R_Basics"))

  while (!is_root(root_cand)) {
    next_cand <- normalizePath(file.path(root_cand, ".."), mustWork = TRUE)
    # if .. has no effect, we have reached the file system root
    if (next_cand == root_cand) {
      cli::cli_abort("Directory \"{path}\" is not inside a lectures directory.",
                   call = error_call)
    }
    root_cand <- next_cand
  }

  root_cand
}


# extract packages that are loaded or installed inside an r file
# This only spots packages attached with library() or require(), not those
# used with the ::-notation
get_used_packages <- function(file, path) {
    pkgs <- file.path(path, file) %>%
      readr::read_lines() %>%
      stringr::str_subset("^ *(library|require|install\\.packages)\\(") %>%
      stringr::str_extract("(library|require|install\\.packages)\\(([^),]+)",
                           group = 2) %>%
      # remove quotes
      stringr::str_remove_all("\"|'") %>%
      unique()
    dplyr::tibble(
      file = file,
      package = pkgs
    )
  }

