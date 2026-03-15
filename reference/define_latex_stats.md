# Define LaTeX commands for statistical symbols

Add the definitions for various useful LaTeX equation symbols for
statistics to an RMarkdown or Quarto document.

## Usage

``` r
define_latex_stats()
```

## Value

The function returns `NULL` invisibly. The command definitions are
output as a side effect.

## Details

Run this function from within a code chunk in a RMarkdown or Quarto
document with options `results = "asis"` and `echo = FALSE` (see
"Examples"). It only works for pdf output.

It defines the following macros: `\E`, `\P`, `\Var`, `\Cov`, `\Cor`,
`\SD`, `\SE`, `\Xb`, `\Yb`.

## Examples
