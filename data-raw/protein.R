library(readr)
library(dplyr)

# read the data as fixed width, add headers manually
url <- "https://raw.githubusercontent.com/jgscott/STA380/master/data/protein.csv"
protein <- read_csv(url) %>%
  rename(
    country = "Country",
    red_meat = "RedMeat",
    white_meat = "WhiteMeat",
    eggs = "Eggs",
    milk = "Milk",
    fish = "Fish",
    cereals = "Cereals",
    starch = "Starch",
    nuts = "Nuts",
    fruit_veg = "Fr&Veg"
  ) %>%
  as_tibble()

usethis::use_data(protein, overwrite = TRUE)
