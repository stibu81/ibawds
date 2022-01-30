library(dplyr)

set.seed(1435)

galton_sons <- HistData::GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  # randomly sample one son per family
  sample_n(1) %>%
  ungroup() %>%
  select(father, son = childHeight) %>%
  # convert to centimeters
  mutate(across(everything(), ~. * 2.54))

galton_daughters <- HistData::GaltonFamilies %>%
  filter(gender == "female") %>%
  group_by(family) %>%
  # randomly sample one daughter per family
  sample_n(1) %>%
  ungroup() %>%
  select(father, daughter = childHeight) %>%
  # convert to centimeters
  mutate(across(everything(), ~. * 2.54))

usethis::use_data(galton_sons, overwrite = TRUE)
usethis::use_data(galton_daughters, overwrite = TRUE)

