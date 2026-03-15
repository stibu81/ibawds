# Evaluate Predictions for the Case Study Handed in By Students

Evaluate Predictions for the Case Study Handed in By Students

## Usage

``` r
evaluate_casestudy(prediction_files, solution_file)
```

## Arguments

- prediction_files:

  character of file paths of csv files with model predictions.

- solution_file:

  path to the parquet file containing the correct solutions.

## Value

a tibble with one row for each file given in `prediction_files` and the
following columns:

- rank:

  the rank of the prediction among all predictions in the tibble The
  tibble is sorted according to rank and ranking occurs first by
  `balanced_accuracy` and then `accuracy`.

- file:

  the name of the file that contained the prediction.

- n_valid:

  the number of valid predictions in the file.

- balanced_accuracy:

  the mean of sensitivity and specificity.

- accuracy:

  accuracy of the prediction.

- sensitivity:

  sensitivity, i.e., the rate of correct predictions for the "positive"
  class `"<=50K"`.

- specificity:

  specificity, i.e., the rate of correct predictions for the "negative"
  class `">50K"`.

## Details

The prediction files must be csv-files (comma separated) with two
columns:

- id:

  a five-digit integer giving the ID of the person.

- class:

  the predicted income class, one of `"<=50K"` and `">50K"`.

Missing IDs and any class that is not one of the accepted values count
as failed predictions. The performance metrics are always computed on
the full data set, not just on the available predictions.
