# History of the Number of Available CRAN Packages

Table with the number of packages available on CRAN and the current R
version for historic dates back to 21 June 2001.

## Usage

``` r
cran_history
```

## Format

A data frame with 76 rows and 4 variables.

- date:

  date

- n_packages:

  the number of available R packages on CRAN

- version:

  the then current version of R

- source:

  source of the data (see 'Details')

## Details

Data on the number of packages on CRAN between 2001-06-21 and 2014-04-13
is obtained from
[`CRANpackages`](https://www.rdocumentation.org/packages/Ecdat/versions/0.3-9/topics/CRANpackages)
from the package [`Ecdat`](https://cran.r-project.org/package=Ecdat).
This data was collected by John Fox and Spencer Graves. Intervals
between data points are irregularly spaced. These data are marked with
"John Fox" or "Spencer Graves" in the column `source`. They are licenced
under GPL-2/GPL-3.

Data between 2014-10-01 and 2023-03-06 was collected by the package
author from CRAN snapshots on Microsoft's MRAN, which was retired on 1
July 2023. Data was collected on the first day of each quarter. These
data are marked with "MRAN" in the column `source`.

Newer data has been collected in irregular intervals using the functions
[`n_available_packages()`](https://stibu81.github.io/ibawds/reference/n_available_packages.md)
and
[`available_r_version()`](https://stibu81.github.io/ibawds/reference/n_available_packages.md).
These data are marked with "CRAN" in the column `source`.

## Examples

``` r
library(ggplot2)
ggplot(cran_history, aes(x = date, y = n_packages)) +
  geom_point()

```
