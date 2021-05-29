library(dplyr)

# get number of available CRAN packages from MRAN for every quarter
# since 2014-10-01. Do not redownload the quarters that are already
# available in the package
start_date <- as.Date("2014-10-01")
cran_history <- get_cran_history() %>%
  filter(date >= start_date)
dates <- seq(start_date, lubridate::today(), by = "3 months") %>%
  as.character %>%
  setdiff(as.character(cran_history$date)) %>%
  as.Date()

if (length(dates) == 0) {
  stop("There is no new data to download.")
}

cran_history_new <- tibble(
  date = dates,
  n_packages = vapply(dates, n_available_packages, integer(1))
)

cran_history <- bind_rows(cran_history, cran_history_new)

usethis::use_data(cran_history, internal = TRUE, overwrite = TRUE)
