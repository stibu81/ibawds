library(tibble)
library(dplyr)

mtcars2 <- as_tibble(mtcars, rownames = "model")

usethis::use_data(mtcars2, overwrite = TRUE)

mtcars2_na <- mtcars2 %>%
  dplyr::mutate(
    mpg = replace(mpg, c(1, 7:9, 15, 25, 30),  NA),
    disp = replace(disp, c(7:9, 17, 20, 28), NA),
    wt = replace(wt, c(1:3, 8, 17, 23), NA),
    qsec = replace(qsec, c(5, 17, 23, 32), NA)
  )

usethis::use_data(mtcars2_na, overwrite = TRUE)
