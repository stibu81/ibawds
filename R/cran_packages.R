#' Number of Available R Packages and R Versions from MRAN
#'
#' MRAN has an archive of Snapshots of CRAN dating back to September 17 2014.
#' These functions return the number of available packages and the available
#' R version according to the snapshot of <https://cran.r-project.org> on
#' [MRAN](https://mran.microsoft.com).
#'
#' @param date the date of the snapshot to be used. It can be a `Date` object
#'  or a character in the format `%Y-%m-%d`.
#'
#' @details
#' MRAN has data starting from September 17 2014. Data for a few selected dates
#' before September 17 2014 can be obtained from the dataset
#' [`CRANpackages`](https://www.rdocumentation.org/packages/Ecdat/versions/0.3-9/topics/CRANpackages)
#' from the package [`Ecdat`](https://cran.r-project.org/package=Ecdat).
#' A more complete dataset ranging from 2001 until
#' today is included in the package as [`cran_history`].
#'
#' Note that for some dates there is no snapshot on MRAN. The function will
#' return an error in those cases.
#'
#' @return
#' the number of available packages as an integer or the R version number as
#' a character
#'
#' @seealso [`cran_history`]
#'
#' @export

n_available_packages <- function(date = Sys.Date()) {
  get_mran_page(date, "packages") %>%
    stringr::str_subset("repository features \\d+ available packages") %>%
    stringr::str_extract("(?<=features )\\d+(?= available)") %>%
    as.integer()
}


#' @rdname n_available_packages
#' @export

available_r_version <- function(date = Sys.Date()) {
  get_mran_page(date, "main") %>%
    stringr::str_subset("R-[0-9.]+\\.tar\\.gz") %>%
    stringr::str_extract("(?<=R-)[0-9.]+(?=\\.tar\\.gz)")
}

# helper function to download a page from MRAN
get_mran_page <- function(date, type) {

  if (is.character(date)) date <- try(as.Date(date), silent = TRUE)

  if (!methods::is(date, "Date")) {
    stop(deparse(substitute(date)), " is not a valid date.")
  }

  # MRAN goes back to 2014-09-17
  if (date < as.Date("2014-09-17")) {
    stop("MRAN has no data for dates before 2014-09-17.\n",
         "Data for some older dates can be ",
         "obtained from Ecdat::CRANpackages.")
  }
  if (date > Sys.Date()) {
    stop("MRAN has no data for dates in the future.")
  }

  # determine the url and download
  url <- if (type == "packages") {
    paste0("https://cran.microsoft.com/snapshot/", date, "/web/packages/")
  } else if (type == "main") {
    paste0("https://cran.microsoft.com/snapshot/", date, "/banner.shtml")
  } else {
    stop("invalid value for type")
  }

  tryCatch(
    page <- readLines(url, n = 40),
    error = function(e) {
      stop(
        simpleError(
          paste("Obtaining data from MRAN failed with error:",
                conditionMessage(e)),
          call = sys.call(-4)
        )
      )
    })

  page
}
