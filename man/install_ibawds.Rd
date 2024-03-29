% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install.R
\name{install_ibawds}
\alias{install_ibawds}
\title{Install the R-Packages Required for the Course}
\usage{
install_ibawds()
}
\value{
nothing or \code{NULL} invisibly
}
\description{
A number of R-packages are used in the courses and
the video lectures. They are also dependencies of
this package. Use \code{install_ibawds()} to install the
packages that are not yet installed.
}
\details{
This function checks whether all the packages that \code{ibawds} depends on,
imports or suggests are installed. In interactive sessions, it either
informs the user that all packages are installed or asks to install
missing packages. The function relies on \code{\link[rlang:is_installed]{rlang::check_installed()}}.
}
