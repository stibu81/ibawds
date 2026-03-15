# Set Options for Slides

Set options for ggplot plots and tibble outputs for IBAW slides.

## Usage

``` r
set_slide_options(
  ggplot_text_size = 22,
  ggplot_margin_pt = rep(10, 4),
  tibble_print_max = 12,
  tibble_print_min = 8
)
```

## Arguments

- ggplot_text_size:

  Text size to be used in ggplot2 plots. This applies to all texts in
  the plots.

- ggplot_margin_pt:

  numeric vector of length 4 giving the sizes of the top, right, bottom,
  and left margins in points.

- tibble_print_max:

  Maximum number of rows printed for a tibble. Set to `Inf` to always
  print all rows.

- tibble_print_min:

  Number of rows to be printed if a tibble has more than
  `tibble_print_max` rows.

## Value

a named list (invisibly) with to elements containing the old values of
the options for the ggplot theme and the base R options, respectively.
These can be used to reset the ggplot theme and the base R options to
their previous values.

## Details

The function uses
[`ggplot2::theme_update()`](https://ggplot2.tidyverse.org/reference/get_theme.html)
to modify the default theme for ggplot and
[`options()`](https://rdrr.io/r/base/options.html) to set base R options
that influence the printing of tibbles.

Note that if you make changes to these options in a R Markdown file, you
may have to delete the knitr cache in order for the changes to apply.
