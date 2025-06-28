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
    dplyr::mutate(class = factor(stringr::str_trim(class)))

  purrr::map(
      prediction_files,
      \(file) evaluate_one_prediction(file, solution)
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(
      dplyr::desc(.data$balanced_accuracy), dplyr::desc(.data$accuracy)
    ) %>%
    dplyr::mutate(
      ba_neg = -.data$balanced_accuracy,
      ac_neg = -.data$accuracy,
      rank = dplyr::min_rank(dplyr::pick(.data$ba_neg, .data$ac_neg)),
      .before = .data$file
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
    missing_pred <- solution %>%
      dplyr::filter(!.data$id %in% pred$id) %>%
      # ensure the predictions are wrong
      dplyr::mutate(class = dplyr::if_else(class == ">50K", "<=50K", ">50K"))
    pred <- dplyr::bind_rows(pred, missing_pred)
  }
  pred$class <- factor(pred$class, levels = c("<=50K", ">50K"))

  # evaluate the relevant metrics
  comp <- solution %>%
    dplyr::full_join(pred, by = "id", suffix = c("_true", "_pred"))
  cm <- caret::confusionMatrix(comp$class_pred, comp$class_true)

  dplyr::tibble(
    file = file,
    n_pred = n_valid_preds,
    balanced_accuracy = cm$byClass["Balanced Accuracy"],
    accuracy = cm$overall["Accuracy"],
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"]
  )
}
