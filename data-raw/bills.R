library(dplyr)

bills <- reshape2::tips %>%
  group_by(sex, time, smoker) %>%
  summarise(mean_bill = mean(total_bill), .groups = "drop") %>%
  mutate(
    smoker = forcats::fct_recode(smoker, Raucher = "Yes", Nichtraucher = "No"),
    time = forcats::fct_recode(time, Mittag = "Lunch", Abend = "Dinner"),
    sex = forcats::fct_recode(sex, Frau = "Female", Mann = "Male")
  )

usethis::use_data(bills, overwrite = TRUE)
