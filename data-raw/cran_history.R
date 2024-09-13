library(ibawds)
library(dplyr)
library(cli)

cran_history <- ibawds::cran_history

if (Sys.Date() %in% cran_history$date) {
  cli_abort("Data for today has already been collected.")
}

cran_history_new <- tibble(
  date = Sys.Date(),
  n_packages = n_available_packages(),
  version = available_r_version(),
  source = "CRAN"
)

cran_history <- bind_rows(cran_history, cran_history_new)

usethis::use_data(cran_history, overwrite = TRUE)

