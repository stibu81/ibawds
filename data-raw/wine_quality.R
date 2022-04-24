library(readr)
library(dplyr)
library(stringr)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
wq_red <- read_delim(url, delim = ";", name_repair = ~ str_replace_all(., " ", "_"))
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
wq_white <- read_delim(url, delim = ";", name_repair = ~ str_replace_all(., " ", "_"))

wine_quality <- list(red = wq_red, white = wq_white) %>%
  bind_rows(.id = "colour")

usethis::use_data(wine_quality, overwrite = TRUE)
