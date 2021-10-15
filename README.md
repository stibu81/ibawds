
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ibawds <img src="man/figures/ibawds_logo.png" align="right" height="175" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ibawds)](https://CRAN.R-project.org/package=ibawds)
[![R-CMD-check](https://github.com/stibu81/ibawds/workflows/R-CMD-check/badge.svg)](https://github.com/stibu81/ibawds/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/stibu81/ibawds/branch/master/graph/badge.svg)](https://app.codecov.io/gh/stibu81/ibawds?branch=master)
<!-- badges: end -->

ibawds provides useful functions and datasets for the [Data Science
course at
IBAW](https://ibaw.ch/bildungsangebote/informatik/data-science/) in
Lucerne.

## Installation

You can install the released version of ibawds from
[CRAN](https://cran.r-project.org/package=ibawds) with:

``` r
install.packages("ibawds")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stibu81/ibawds")
```

## Setting up for the course

### Install R

Windows and Mac Users can download the installer for their operating
system from \[<https://cran.r-project.org/>\]. For Windows, installation
should be possible also for non-admin users.

Linux users find instructions for installation under Debian,
Fedora/Redhat, and Ubuntu under the same link. Note that the official
package sources may contain an outdated version of R, but usually the
current version can be installed from some other source. Make sure that
you install version 4.0 or newer.

### Install RStudio

We will use RStudio as our
[IDE](https://en.wikipedia.org/wiki/Integrated_development_environment)
in the course. You can download it from this
[link](https://www.rstudio.com/products/rstudio/download/#download).
Usually, it is easiest to use the appropriate installer for your
operating system, however, if you don’t have admin access on your
computer, you should instead download the appropriate zip-file /
tarball.

When you run RStudio for the first time, you have to select the R
version that you want to use.

### Installing R Packages

We will use a variety of R Packages during the course. To set them up,
first install ibawds with

``` r
install.packages("ibawds")
```

and then run the following to install additional packages:

``` r
library(ibawds)
install_ibawds()
```

You will be asked to install missing packages. Confirm to install them.
Note that this may take more than 30 minutes depending on your system
and configuration.
