library(tibble)

mtcars2 <- as_tibble(mtcars, rownames = "model")

usethis::use_data(mtcars2, overwrite = TRUE)
