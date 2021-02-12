#' Define LaTeX commands for statistical symbols
#'
#' Use this function to add the definitions for various useful
#' equation symbols for statistics to a RMarkdown document.
#'
#' @details
#' Run this function from within a code chunk with options
#' `results = "asis"` and `echo = FALSE`.
#'
#' It adds the following commands: `\E`, `\P`, `\Var`,
#' `\SD`, `\SE`, `\Xb`, `\Yb`
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
