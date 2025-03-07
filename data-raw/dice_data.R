# generate sampled dice data for multiple dice. Some of the dice are not fair.
# To make sure that the students cannot look up the correct solution, the
# input probabilities are encrypted.
# The password is stored as the variable PW_PROBS_DICE_DATA in the repository.

library(dplyr)
library(stringr)
library(purrr)
library(safer)

password <- "PW_PROBS_DICE_DATA"

n <- c(d1 = 174, d2 = 158, d3 = 207, d4 = 168, d5 = 1027, d6 = 784)

# use this snippet to create encrypted probabilities
# "1|1|1|1|1|1;
# 1|1|1|1|1|1;
# 1|1|1|1|1|1;
# 1|1|1|1|1|1;
# 1|1|1|1|1|1;
# 1|1|1|1|1|1;" %>%
#   encrypt_string(key = password) %>%
#   str_extract_all(".{1,65}") %>%
#   unlist() %>%
#   dput()

probs <- paste0(
    "s7okNAnrJHXkTrY8EjgDWwhREMWbhSlIbjMPf/m6ueS5n1j/iUhmSfi/+XQnw4UWJ",
    "NiZu5F5sJiv6fO8XEe5wvCaEYzy/tnpTxzZSh8MH0gJrhAlT4Xv6Hrv2Ksm32IRTG",
    "dwsyEL/P4Jf3AWjGq3NPokZd4bDJyVaJEc6UqyYltwbb6ZprpMucmGig=="
  ) %>%
  decrypt_string(key = password) %>%
  str_split_1(";") %>%
  str_split("\\|") %>%
  map(as.numeric) %>%
  set_names(names(n))

set.seed(3451)
dice_data <- map2(
  n, probs,
  \(n, probs) sample(1:6, n, replace = TRUE, probs)
)

usethis::use_data(dice_data, overwrite = TRUE)
