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
#' [`Ecdat::CRANpackages`]. A more complete dataset ranging from 2001 until
#' today can be obtained with [`get_cran_history()`].
#'
#' Note that for some dates there is no snapshot on MRAN. The function will
#' return an error in those cases.
#'
#' @return
#' the number of available packages as an integer or the R version number as
#' a character
#'
#' @seealso [`get_cran_history()`]
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


#' History of the Number of Available CRAN Packages
#'
#' Get a data frame containing the number of packages available for historic
#' dates back to 21 June 2001.
#'
#' @details
#' Data on the number of packages on CRAN between 2001-06-21 and 2014-04-13
#' is obtained from [`Ecdat::CRANpackages`].
#' This data was collected by John Fox and Spencer Graves. Intervals between
#' data points are irregularly spaced.
#'
#' Newer data was obtained using the function [`n_available_packages()`] which
#' extracts the information from CRAN snapshots on MRAN. One data point per
#' quarter is available starting on 2014-10-01.
#'
#' @return
#' a tibble with columns `date` and `n_packages`
#'
#' @examples
#' library(ggplot2)
#' cran_history <- get_cran_history()
#' ggplot(cran_history, aes(x = date, y = n_packages)) +
#'   geom_point()
#'
#' @export

get_cran_history <- function() {

  Ecdat::CRANpackages %>%
    dplyr::as_tibble() %>%
    dplyr::select(date = "Date", n_packages = "Packages",
                  version = "Version", source = "Source") %>%
    dplyr::mutate(source = stringr::str_trim(.data$source)) %>%
    dplyr::bind_rows(cran_history)
}
