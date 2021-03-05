#' Define LaTeX commands for statistical symbols
#'
#' Add the definitions for various useful LaTeX
#' equation symbols for statistics to a RMarkdown document..
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
