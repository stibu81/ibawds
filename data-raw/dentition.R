library(readr)
library(dplyr)
library(stringr)

# read the data as fixed width, add headers manually
url <- "https://people.sc.fsu.edu/~jburkardt/datasets/hartigan/file19.txt"
dentition <- read_fwf(
    url,
    skip = 21,
    col_positions = fwf_cols(name = 18, I = 2, i = 2,
                             C = 2, c = 2, P = 2, p = 2,
                             M = 2, m = 1)
  ) %>%
  mutate(name = str_remove_all(name, "\""))

usethis::use_data(dentition, overwrite = TRUE)
