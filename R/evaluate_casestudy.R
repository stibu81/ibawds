#' Evaluate Predictions for the Case Study Handed in By Students
#'
#' @param prediction_files character of file paths of csv files with
#'  model predictions.
#' @param solution_file path to the parquet file containing the correct
#'    solutions.
#'
#' @details
#' The prediction files must be csv-files (comma separated) with two columns:
#'
#' \describe{
#'   \item{id}{a five-digit integer giving the ID of the person.}
#'   \item{class}{the predicted income class, one of `"<=50K"` and `">50K"`.}
#' }
#'
#' Missing IDs and any class that is not one of the accepted values count as
#' failed predictions. The performance metrics are always computed on the
#' full data set, not just on the available predictions.
#'
#' @return
#' a tibble with one row for each file given in `prediction_files` and the
#' following columns:
#'
#' \describe{
#'   \item{rank}{the rank of the prediction among all predictions in the tibble
#'     The tibble is sorted according to rank and ranking occurs first by
#'     `balanced_accuracy` and then `accuracy`.}
#'   \item{file}{the name of the file that contained the prediction.}
#'   \item{n_valid}{the number of valid predictions in the file.}
#'   \item{balanced_accuracy}{the mean of sensitivity and specificity.}
#'   \item{accuracy}{accuracy of the prediction.}
#'   \item{sensitivity}{sensitivity, i.e., the rate of correct predictions for
#'     the "positive" class `"<=50K"`.}
#'   \item{specificity}{specificity, i.e., the rate of correct predictions for
#'     the "negative" class `">50K"`.}
#' }
#'
#' @export

evaluate_casestudy <- function(prediction_files, solution_file) {

  rlang::check_installed("nanoparquet")

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

  pred <- prepare_predictions(file, solution)
  metrics <- compute_metrics(pred, solution)

  dplyr::tibble(
    file = file,
    n_valid = metrics$n_valid,
    balanced_accuracy = (metrics$sensitivity + metrics$specificity) / 2,
    accuracy = metrics$accuracy,
    sensitivity = metrics$sensitivity,
    specificity = metrics$specificity
  )
}


prepare_predictions <- function(file, solution) {

  pred <- readr::read_csv(file, col_types = "ic")

  # warn if there are problems
  bad_id <- !pred$id %in% solution$id
  if (any(bad_id)) {
    cli::cli_warn(
      c("x" = "The file {file} contains invalid ids that are removed.")
    )
    pred <- pred[!bad_id, ]
  }
  if (any(is.na(pred$class))) {
    cli::cli_warn(
      c("x" = "The file {file} contains predictions that are NA.")
    )
  }
  if (any(!pred$class %in% unique(solution$class))) {
    cli::cli_warn(
      c("x" = "The file {file} contains invalid predictions.")
    )
  }
  if (!setequal(solution$id, pred$id)) {
    cli::cli_warn(
      c("x" = "The file {file} does not contain predictions for all ids.")
    )
  }

  pred
}


compute_metrics <- function(pred, solution) {

  comp <- solution %>%
    dplyr::inner_join(pred, by = "id", suffix = c("_true", "_pred")) %>%
    tidyr::drop_na()
  list(
    n_valid = sum(pred$class %in% c("<=50K", ">50K"), na.rm = TRUE),
    accuracy = sum(comp$class_true == comp$class_pred) / nrow(solution),
    sensitivity = sum(comp$class_true == "<=50K" & comp$class_pred == "<=50K") /
      sum(solution$class == "<=50K"),
    specificity = sum(comp$class_true == ">50K" & comp$class_pred == ">50K") /
      sum(solution$class == ">50K")
  )
}
