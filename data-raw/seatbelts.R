library(dplyr)

dates <- time(Seatbelts) %>%
  as.vector() %>%
  lubridate::date_decimal() %>%
  lubridate::round_date(unit = "month") %>%
  as.Date()

seatbelts <- as_tibble(Seatbelts) %>%
  mutate(date = dates) %>%
  select(date, drivers, front, rear) %>%
  tidyr::pivot_longer(-date,
                      names_to = "seat",
                      values_to = "victims") %>%
  mutate(seat = forcats::fct_recode(
    seat,
    Fahrer = "drivers",
    Beifahrer = "front",
    "R\u00fccksitz" = "rear"
  ))

usethis::use_data(seatbelts, overwrite = TRUE)
