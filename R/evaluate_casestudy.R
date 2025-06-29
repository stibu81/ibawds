#' Evaluate Predictions for the Case Study Handed in By Students
#'
#' @param prediction_files character of file paths of csv files with
#'  model predictions.
#' @param solution_file path to the parquet file containing the correct
#'    solutions.
#'
#' @export

evaluate_casestudy <- function(prediction_files, solution_file) {

  rlang::check_installed("caret")

  solution <- nanoparquet::read_parquet(solution_file) %>%
    dplyr::select("id", "class") %>%
    dplyr::mutate(class = stringr::str_trim(class))

  purrr::map(
      prediction_files,
      \(file) evaluate_one_prediction(file, solution)
    ) %>%
    dplyr::bind_rows() %>%
    # sort and rank
    dplyr::arrange(
      dplyr::desc(.data$balanced_accuracy), dplyr::desc(.data$accuracy)
    ) %>%
    dplyr::mutate(
      ba_neg = -.data$balanced_accuracy,
      ac_neg = -.data$accuracy,
      rank = dplyr::min_rank(dplyr::pick("ba_neg", "ac_neg")),
      .before = "file"
    ) %>%
    dplyr::select(-dplyr::contains("_neg"))
}


evaluate_one_prediction <- function(file, solution) {

  pred <- readr::read_csv(file, col_types = "ic")

  # remove all rows that have problems
  bad_id <- !pred$id %in% solution$id
  if (any(bad_id)) {
    cli::cli_warn(
      c("x" = "The file {file} contains invalid ids that are removed.")
    )
    pred <- pred[!bad_id, ]
  }
  na_class <- is.na(pred$class)
  if (any(na_class)) {
    cli::cli_warn(
      c("x" = "The file {file} contains predictions that are NA. They are removed.")
    )
    pred <- pred[!na_class, ]
  }
  bad_class <- !pred$class %in% unique(solution$class)
  if (any(bad_class)) {
    cli::cli_warn(
      c("x" = "The file {file} contains invalid predictions. They are removed.")
    )
    pred <- pred[!bad_class, ]
  }

  # check if there are predictions for all ids. Missing predictions must be
  # set to the wrong value by hand, because missing values are ignored when
  # evaluating the performance metrics.
  n_valid_preds <- nrow(pred)
  if (!setequal(solution$id, pred$id)) {
    cli::cli_warn(
      c("x" = "The file {file} does not contain valid predictions for all ids.")
    )
  }

  # evaluate the relevant metrics
  comp <- solution %>%
    dplyr::inner_join(pred, by = "id", suffix = c("_true", "_pred"))
  accuracy <- sum(comp$class_true == comp$class_pred) / nrow(solution)
  sensitivity <- sum(comp$class_true == "<=50K" & comp$class_pred == "<=50K") /
    sum(solution$class == "<=50K")
  specificity <- sum(comp$class_true == ">50K" & comp$class_pred == ">50K") /
    sum(solution$class == ">50K")

  dplyr::tibble(
    file = file,
    n_pred = n_valid_preds,
    balanced_accuracy = (sensitivity + specificity) / 2,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity
  )
}
