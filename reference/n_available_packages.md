# Number of Available R Packages and R Versions from CRAN

Obtain the number of available packages on CRAN and the current R
version.

## Usage

``` r
n_available_packages(cran = getOption("repos"))

available_r_version(cran = getOption("repos"))
```

## Arguments

- cran:

  character vector giving the base URL of the CRAN server to use.

## Value

the number of available packages as an integer or the R version number
as a character

## Details

The number of packages on CRAN and the R version can be obtained for
selected dates in the past from the dataset
[`cran_history`](https://stibu81.github.io/ibawds/reference/cran_history.md).

**Note:** Previously, these functions could obtain the number of
packages on CRAN and the then current R version also for past dates by
using snapshots from Microsoft's MRAN. However, MRAN shut down on 1 July
2023 such that this functionality is no longer available.

## See also

[`cran_history`](https://stibu81.github.io/ibawds/reference/cran_history.md)
