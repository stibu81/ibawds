library(readr)
library(dplyr)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
col_names <- c("id", "clump_thick", "unif_cell_size", "unif_cell_shape",
               "marg_adh", "ep_cell_size", "bare_nucl",
               "bland_chromat", "norm_nucl", "mitoses", "class")
breast_cancer <- read_delim(url, delim = ",", na = c("?"), col_names = col_names,
                            col_types = cols(id = "c", .default = "i")) %>%
  mutate(class = factor(class,
                        levels = c(2, 4),
                        labels = c("benign", "malignant")))

usethis::use_data(breast_cancer, overwrite = TRUE)
