# Dataset mtcars without row names

In the [`mtcars`](https://rdrr.io/r/datasets/mtcars.html) dataset, the
names of the car models are stored as row names. However, when working
with `ggplot2` and other packages from the `tidyverse`, it is convenient
to have all data in columns. `mtcars2` is a variant of `mtcars` that
contains car models in a column instead of storing them as row names.
`mtcars_na` is the same dataset as `mtcars2`, but some of the columns
contain missing values.

## Usage

``` r
mtcars2

mtcars2_na
```

## Format

A data frame with 32 rows and 12 variables. The format is identical to
[`mtcars`](https://rdrr.io/r/datasets/mtcars.html) and details can be
found in its documentation. The only difference is that the car model
names are stored in the column `model` instead of the row names.
