#' Define LaTeX commands for statistical symbols
#'
#' Add the definitions for various useful LaTeX
#' equation symbols for statistics to an RMarkdown document.
#'
#' @details
#' Run this function from within a code chunk in a RMarkdown document
#' with options `results = "asis"` and `echo = FALSE` (see "Examples").
#' It only works for pdf output.
#'
#' It defines the following macros: `\E`, `\P`, `\Var`, `\Cov`, `\Cor`,
#' `\SD`, `\SE`, `\Xb`, `\Yb`.
#'
#' @return
#' The function returns `NULL` invisibly. The command definitions
#' are output as a side effect.
#'
#' @examples \dontrun{
#' # add this code chunk to a RMarkdown document
#' ```{r results = "asis", echo = FALSE}
#'   define_latex_stats()
#' ```
#' }
#'
#' @export

define_latex_stats <- function() {

  cat("\\newcommand{\\E}{\\mbox{E}}",
      # original \P stands for pilcrow
      "\\renewcommand{\\P}{\\mbox{P}}",
      "\\newcommand{\\Var}{\\mbox{Var}}",
      "\\newcommand{\\Cov}{\\mbox{Cov}}",
      "\\newcommand{\\Cor}{\\mbox{Cor}}",
      "\\newcommand{\\SD}{\\mbox{SD}}",
      "\\newcommand{\\SE}{\\mbox{SE}}",
      "\\newcommand{\\Xb}{\\bar{X}}",
      "\\newcommand{\\Yb}{\\bar{Y}}",
      sep = "\n")

  invisible(NULL)
}


#' Set Options for Slides
#'
#' Set options for ggplot plots and tibble outputs for IBAW slides.
#'
#' @param ggplot_text_size Text size to be used in ggplot2 plots.
#'  This applies to all texts in the plots.
#' @param ggplot_margin_pt numeric vector of length 4 giving the sizes of the
#'  top, right, bottom, and left margins in points.
#' @param tibble_print_max Maximum number of rows printed for a tibble. Set
#'  to `Inf` to always print all rows.
#' @param tibble_print_min Number of rows to be printed if a tibble has more
#'  than `tibble_print_max` rows.
#'
#' @details
#' The function uses [`ggplot2::theme_update()`] to modify the default theme
#' for ggplot and [`options()`] to set base R options that influence the printing
#' of tibbles.
#'
#' Note that if you make changes to these options in a R Markdown file, you may
#' have to delete the knitr cache in order for the changes to apply.
#'
#' @return
#' a named list (invisibly) with to elements containing the old values of the
#' options for the ggplot theme and the base R options, respectively. These can
#' be used to reset the ggplot theme and the base R options to their previous
#' values.
#'
#' @export

set_slide_options <- function(ggplot_text_size = 22,
                              ggplot_margin_pt = rep(10, 4),
                              tibble_print_max = 12,
                              tibble_print_min = 8) {

  ggplot_opts <- ggplot2::theme_update(
    text = ggplot2::element_text(size = ggplot_text_size),
    plot.margin = ggplot2::unit(ggplot_margin_pt, "pt")
  )
  base_opts <- options(pillar.print_max = tibble_print_max,
                       pillar.print_min = tibble_print_min)

  invisible(list(ggplot = ggplot_opts, base = base_opts))
}
