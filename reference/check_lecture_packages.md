# Find Packages Used For Lectures not Installed by ibawds

ibawds offers the function
[`install_ibawds()`](https://stibu81.github.io/ibawds/reference/install_ibawds.md)
which installs all the packages that are required for the course.
`check_lecture_packages()` finds all the packages that are used in the
slides and exercise solution inside a directory. It then checks whether
they are all installed by
[`install_ibawds()`](https://stibu81.github.io/ibawds/reference/install_ibawds.md)
and returns a tibble of those that are not. This can help to identify,
if additional packages need to be installed by
[`install_ibawds()`](https://stibu81.github.io/ibawds/reference/install_ibawds.md).

## Usage

``` r
check_lecture_packages(path = ".")
```

## Arguments

- path:

  the path to a folder inside the directory with the slides and exercise
  solutions. The function automatically tries to identify the top level
  directory of the course material.

## Value

a tibble with two columns:

- file:

  the file where the package is used

- package:

  the name of the package
