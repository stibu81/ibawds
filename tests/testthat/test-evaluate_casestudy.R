library(dplyr, warn.conflicts = FALSE)
library(stringr)

# define the paths to the test files
sol_file <- test_path("data/casestudy/solution.parquet")
ok_file <- test_path("data/casestudy/pred_ok.csv")
nok_file <- test_path("data/casestudy/pred_nok.csv")
errors_file <- test_path("data/casestudy/pred_errors.csv")

# create the reference data
ref_all <- tibble(
    rank = 1:3,
    file = c(ok_file, nok_file, errors_file),
    n_pred = c(50, 40, 32),
    accuracy = c(41, 33, 27) / 50,
    sensitivity = c(22, 17, 15) / 28,
    specificity = c(19, 16, 12) / 22
  ) %>%
  mutate(balanced_accuracy = (sensitivity + specificity) / 2,
         .after = "n_pred")


test_that("evaluate_casestudy() works for a perfect file", {
  ref <- ref_all %>%
    filter(file == ok_file) %>%
    mutate(rank = 1)
  expect_equal(evaluate_casestudy(ok_file, sol_file), ref)
})


test_that("evaluate_casestudy() works for an incomplete file", {
  ref <- ref_all %>%
    filter(file == nok_file) %>%
    mutate(rank = 1)
  expect_equal(evaluate_casestudy(nok_file, sol_file), ref) %>%
    expect_warning(str_glue("{nok_file} does not contain valid predictions"))
})


test_that("evaluate_casestudy() works for a file with errors", {
  ref <- ref_all %>%
    filter(file == errors_file) %>%
    mutate(rank = 1)
  expect_equal(evaluate_casestudy(errors_file, sol_file), ref) %>%
    expect_warning(str_glue("{errors_file} does not contain valid")) %>%
    expect_warning(str_glue("{errors_file} contains invalid ids")) %>%
    expect_warning(str_glue("{errors_file} contains invalid predictions")) %>%
    expect_warning(str_glue("{errors_file} contains predictions that are NA"))
})


test_that("evaluate_casestudy() works with multiple files", {
  expect_equal(
    evaluate_casestudy(
      c(ok_file, nok_file, errors_file), sol_file
    ),
    ref_all
  ) %>%
  expect_warning(str_glue("{nok_file} does not contain valid")) %>%
  expect_warning(str_glue("{errors_file} does not contain valid")) %>%
  expect_warning(str_glue("{errors_file} contains invalid ids")) %>%
  expect_warning(str_glue("{errors_file} contains invalid predictions")) %>%
  expect_warning(str_glue("{errors_file} contains predictions that are NA"))
})


test_that("evaluate_casestudy() works with draws", {
  pred_files <- c(ok_file, nok_file, errors_file, ok_file, errors_file)
  ref <- ref_all %>%
    full_join(tibble(file = pred_files), by = "file") %>%
    mutate(rank = c(1, 1, 3, 4, 4))
  expect_equal(
    suppressWarnings(evaluate_casestudy(pred_files, sol_file)),
    ref
  )
})

