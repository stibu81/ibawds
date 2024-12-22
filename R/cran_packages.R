#' Number of Available R Packages and R Versions from CRAN
#'
#' Obtain the number of available packages on CRAN and the current R version.
#'
#' @param cran character vector giving the base URL of the CRAN server to use.
#'
#' @details
#' The number of packages on CRAN and the R version can be obtained for selected
#' dates in the past from the dataset [`cran_history`].
#'
#' **Note:** Previously, these functions could obtain the number of packages on
#' CRAN and the then current R version also for past dates by using snapshots
#' from Microsoft's MRAN. However, MRAN shut down on 1 July 2023 such that this
#' functionality is no longer available.
#'
#' @return
#' the number of available packages as an integer or the R version number as
#' a character
#'
#' @seealso [`cran_history`]
#'
#' @export

n_available_packages <- function(cran = getOption("repos")) {
  get_cran_page("packages", cran) %>%
    stringr::str_subset("repository features \\d+ available packages") %>%
    stringr::str_extract("(?<=features )\\d+(?= available)") %>%
    as.integer()
}


#' @rdname n_available_packages
#' @export

available_r_version <- function(cran = getOption("repos")) {
  get_cran_page("main", cran) %>%
    stringr::str_subset("R-[0-9.]+\\.tar\\.gz") %>%
    stringr::str_extract("(?<=R-)[0-9.]+(?=\\.tar\\.gz)")
}

# helper function to download a page from CRAN
get_cran_page <- function(type,
                          cran = getOption("repos"),
                          error_call = rlang::caller_env()) {

  # determine the url and download
  url <- if (type == "packages") {
    paste0(cran, "/web/packages/")
  } else if (type == "main") {
    paste0(cran, "/banner.shtml")
  } else {
    cli::cli_abort("invalid value for type")
  }

  tryCatch(
    page <- readLines(url, n = 80),
    error = function(e) {
      cli::cli_abort(
        paste("Obtaining data from CRAN failed with error:",
              conditionMessage(e)),
        call = error_call
      )
    })

  page
}
