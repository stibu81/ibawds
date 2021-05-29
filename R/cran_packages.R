#' Number of Available R Packages from MRAN
#'
#' MRAN has an archive of Snapshots of CRAN dating back to September 17 2014.
#' This function returns the number of available packages according to the
#' snapshot of <https://cran.r-project.org> on [MRAN](https://mran.microsoft.com).
#'
#' @param date the date of the snapshot to be used. It can be a `Date` object
#'  or a character in the format `%Y-%m-%d`.
#'
#' @details
#' Data for a few selected dates before September 17 2014 can be obtained
#' from the dataset [`Ecdat::CRANpackages`].
#'
#' @return
#' the number of available packages as an integer
#'
#' @seealso [`get_cran_history()`]
#'
#' @export

n_available_packages <- function(date) {

  if (is.character(date)) date <- as.Date(date)

  if (!methods::is(date, "Date")) {
    stop(deparse(substitute(date)), " is not a valid date.")
  }

  # MRAN goes back to 2014-09-17
  if (date < as.Date("2014-09-17")) {
    stop("MRAN has no data for dates before 2014-09-17.\n",
         "Data on some dates prior to that date can ",
         "be obtained from Ecdat::CRANpackages.")
  }
  if (date > Sys.Date()) {
    stop("MRAN has no data for dates in the future.")
  }

  # download the page and extract the number of packages
  url <- paste0("https://cran.microsoft.com/snapshot/",
                date, "/web/packages/")
  tryCatch(
    page <- readLines(url, n = 20),
    error = function(e) {
      stop("Obtaining data from MRAN failed with error ",
           e$message)
    })

  page %>%
    stringr::str_subset("repository features \\d+ available packages") %>%
    stringr::str_extract("(?<=features )\\d+(?= available)") %>%
    as.integer()
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
    dplyr::select(date = "Date", n_packages = "Packages") %>%
    dplyr::bind_rows(cran_history)
}
