# Tables Used for Grading the Papers

These functions create two tables that can be used for the grading of
the student's papers.

## Usage

``` r
create_minreq_table(
  repro,
  n_tab,
  n_plot_kinds,
  n_plots,
  n_stat,
  lang = c("de", "en")
)

create_grading_table(
  p_text,
  p_tab,
  p_plot,
  p_code,
  p_stat,
  lang = c("de", "en")
)
```

## Arguments

- repro:

  logical, is the paper reproducible?

- n_tab:

  integer, number of tables

- n_plot_kinds:

  integer, number of different kinds of plots

- n_plots:

  integer, number of plots

- n_stat:

  integer, number of statistical computations

- lang:

  language to use in the tables. Supported languages are German (`"de"`,
  the default) and English ("`en"`).

- p_text:

  numeric between 0 and 3, points given for the text

- p_tab:

  numeric between 0 and 3, points given for the tables

- p_plot:

  numeric between 0 and 5, points given for the plots

- p_code:

  numeric between 0 and 5, points given for the code

- p_stat:

  numeric between 0 and 5, points given for the statistic computations

## Value

both functions return an object of class `kableExtra`.

## Details

The tables are created using
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) and
[`kableExtra::kableExtra`](https://rdrr.io/pkg/kableExtra/man/kableExtra-package.html)
is used for additional styling.

`create_minreq_table()` creates a table that checks that the minimal
requirements are satisfied:

- the paper must be reproducible

- there must be at least one formatted table

- there must be at least 5 plots of at least three different types

- there must be at least two statistical computations

The table lists for each of those requirement whether it is satisfied or
not.

`create_grading_table()` creates a table that gives grades in percent
for each of five categories:

- Text

- Tables

- Plots

- Code

- Statistical computations

In each category, up to five points may be awarded. The last row of the
table gives the percentage over all categories.
