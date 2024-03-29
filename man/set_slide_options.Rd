% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slide_tools.R
\name{set_slide_options}
\alias{set_slide_options}
\title{Set Options for Slides}
\usage{
set_slide_options(
  ggplot_text_size = 22,
  ggplot_margin_pt = rep(10, 4),
  tibble_print_max = 12,
  tibble_print_min = 8
)
}
\arguments{
\item{ggplot_text_size}{Text size to be used in ggplot2 plots.
This applies to all texts in the plots.}

\item{ggplot_margin_pt}{numeric vector of length 4 giving the sizes of the
top, right, bottom, and left margins in points.}

\item{tibble_print_max}{Maximum number of rows printed for a tibble. Set
to \code{Inf} to always print all rows.}

\item{tibble_print_min}{Number of rows to be printed if a tibble has more
than \code{tibble_print_max} rows.}
}
\value{
a named list (invisibly) with to elements containing the old values of the
options for the ggplot theme and the base R options, respectively. These can
be used to reset the ggplot theme and the base R options to their previous
values.
}
\description{
Set options for ggplot plots and tibble outputs for IBAW slides.
}
\details{
The function uses \code{\link[ggplot2:theme_get]{ggplot2::theme_update()}} to modify the default theme
for ggplot and \code{\link[=options]{options()}} to set base R options that influence the printing
of tibbles.

Note that if you make changes to these options in a R Markdown file, you may
have to delete the knitr cache in order for the changes to apply.
}
