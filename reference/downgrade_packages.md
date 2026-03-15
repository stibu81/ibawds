# Downgrade Packages to an Older Version

Downgrade packages to an older version available on CRAN. This can be
useful when debugging problems that might have arisen due to a package
update.

## Usage

``` r
downgrade_packages(pkg, dec_version = c("any", "patch", "minor", "major"))
```

## Arguments

- pkg:

  character with the names of the packages to be downgraded.

- dec_version:

  character giving the version to decrease. Possible values are "any",
  "patch", "minor", and "major". See 'Details'.

## Value

A character vector with the names of the downgraded packages, invisibly.

## Details

Using the argument `dec_version`, the user can control which version
will be installed. The possible values are:

- `"any"`:

  The previous available version will be installed.

- `"patch"`:

  The newest available version with a smaller patch version number will
  be installed. For packages with three version numbers, this is the
  same as using `"any"`.

- `"minor"`:

  The newest available version with a smaller minor version number will
  be installed.

- `"major"`:

  The newest available version with a smaller major version number will
  be installed.

Downgrading is only possible for packages that are currently installed.
For packages that are not installed, a warning is issued.

The function uses
[`remotes::install_version()`](https://remotes.r-lib.org/reference/install_version.html)
to install a version of a package that is older than the currently
installed version.
