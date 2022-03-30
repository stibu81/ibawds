library(ibawds)
library(dplyr)
library(Ecdat)

# set initialise to TRUE in order to create the data set from scratch
initialise <- FALSE

if (initialise) {
  # data until 2014-04-13 comes from Ecdat. This data is licenced under GPL-3.
  cran_history <- Ecdat::CRANpackages %>%
    dplyr::as_tibble() %>%
    dplyr::select(date = "Date", n_packages = "Packages",
                  version = "Version", source = "Source") %>%
    dplyr::mutate(source = stringr::str_trim(.data$source))
} else {
  cran_history <- ibawds::cran_history
}

# get number of available CRAN packages from MRAN for every quarter
# since 2014-10-01. Do not redownload the quarters that are already
# available in the package
start_date <- as.Date("2014-10-01")
dates <- seq(start_date, lubridate::today(), by = "3 months") %>%
  as.character %>%
  setdiff(as.character(cran_history$date)) %>%
  as.Date()

if (length(dates) == 0) {
  stop("There is no new data to download.", call. = FALSE)
}

cran_history_new <- tibble(
  date = dates,
  n_packages = vapply(dates, n_available_packages, integer(1)),
  version = vapply(dates, available_r_version, character(1)),
  source = "MRAN"
)

cran_history <- bind_rows(cran_history, cran_history_new)

usethis::use_data(cran_history, overwrite = TRUE)

