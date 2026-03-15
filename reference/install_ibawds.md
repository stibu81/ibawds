# Install the R-Packages Required for the Course

A number of R-packages are used in the courses and the video lectures.
They are also dependencies of this package. Use `install_ibawds()` to
install the packages that are not yet installed.

## Usage

``` r
install_ibawds()
```

## Value

nothing or `NULL` invisibly

## Details

This function checks whether all the packages that `ibawds` depends on,
imports or suggests are installed. In interactive sessions, it either
informs the user that all packages are installed or asks to install
missing packages. The function relies on
[`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html).
