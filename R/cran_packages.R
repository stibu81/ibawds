#' Number of Available R Packages from MRAN
#'
#' MRAN has an archive of Snapshots of CRAN dating back to September 17 2014.
#' This function returns the number of available packages according to the
#' snapshot of <https://cran.r-project.org> on MRAN.
#'
#' @param date the date of the snapshot to be used. It can be a `Date` object
#'  or a characer in the format `%Y-%m-%d`.
#'
#' @details
#' Data for a few selected dates before September 17 2014 can be obtained
#' from the dataset [`Ecdat::CRANpackages`].
#'
#' @return
#' the nummber of available packages as an integer
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
