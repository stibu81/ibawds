#' Dataset mtcars without row names
#'
#' In the [`mtcars`] dataset, the names of the car models are
#' stored as row names. However, when working with `ggplot2` and other
#' packages from the `tidyverse`, it is convenient to have all data in columns.
#' `mtcars2` is a variant of `mtcars` that contains car models in a column
#' instead of storing them as row names.
#'
#' @format A data frame with 32 rows and 12 variables. The format is identical
#'  to [`mtcars`] and details can be found in its documentation. The only difference
#'  is that the car model names are stored in the column `model` instead of
#'  row names.
#'
#' @keywords datasets

"mtcars2"


#' Summarised Data on Restaurant Bills
#'
#' Summary of data on restaurant bills from the dataset [`reshape2::tips`].
#' Labels are in German.
#'
#' @format A data frame with 8 rows and 4 variables:
#' \describe{
#'   \item{sex}{sex of the bill payer}
#'   \item{time}{time of day}
#'   \item{smoker}{whether there were smokers in the party}
#'   \item{mean_bill}{mean of all the bills in dollars}
#' }

"bills"


#' Road Casualties in Great Britain 1969-84
#'
#' Extract of the data in the [`Seatbelts`] dataset as a data frame. The
#' original dataset is a multiple time series (class `mts`). Labels are
#' in German.
#'
#' @format A data frame with 576 rows and 3 variables:
#' \describe{
#'   \item{date}{data of the first data of the month for which the
#'     data was collected.}
#'   \item{seat}{seat where the persons that were killed or seriously
#'     injured were seated. One of "Fahrer" (driver's seat), "Beifahrer"
#'     (front seat), "Rücksitz" (rear seat).}
#'   \item{victims}{number of persons that were killed or seriously injured.}
#' }

"seatbelts"
