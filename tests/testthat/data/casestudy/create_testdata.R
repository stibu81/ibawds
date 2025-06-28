# Create the testdata for the casestudy:
# * a parquet file with the correct solution
# * a csv file with a complete prediction without errors
# * a csv file with an incomplete prediction without errors
# * a csv file with an incomplete prediction and some errors (invalid labels)

library(tidyverse)
library(nanoparquet)

dir <- devtools::package_file("tests/testthat/data/casestudy/")

set.seed(75968)
n <- 50

# the ids in the original data have 5 digits, but here I use ids with 4 digits
# only to avoid any implication that these are a subset of the actual data.
solution <- tibble(
  id = sample(1000:9999, size = n),
  class = sample(c("<=50K", ">50K"), size = n, replace = TRUE)
)
write_parquet(solution, file.path(dir, "solution.parquet"))

# create complete prediction without errors. Make sure that the model is
# better than a random model.
pred_ok <- solution %>%
  mutate(class = if_else(runif(n) > 0.6, sample(class), class))
write_csv(pred_ok, file.path(dir, "pred_ok.csv"))

# create an incomplete prediction without errors
pred_nok <- pred_ok %>%
  slice_sample(prop = .8)
write_csv(pred_nok, file.path(dir, "pred_nok.csv"))

# insert some errors
pred_errors <- pred_nok
pred_errors$id[c(12, 34)] <- sample(setdiff(1000:9999, solution$class), 2)
pred_errors$class[c(7, 18, 37)] <- NA_character_
pred_errors$class[c(5, 15, 35)] <- "bad"
write_csv(pred_errors, file.path(dir, "pred_erros.csv"))
