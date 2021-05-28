#' Tables Used for Grading the Papers
#'
#' These functions create two tables that can be used for the grading
#' of the student's papers.
#'
#' @param repro logical, is the paper reproducible?
#' @param n_tab integer, number of tables
#' @param n_plot_kinds integer, number of different kinds of plots
#' @param n_plots integer, number of plots
#' @param n_stat integer, number of statistical computations
#' @param p_text numeric between 0 and 5, points given for the text
#' @param p_tab numeric between 0 and 5, points given for the tables
#' @param p_plot numeric between 0 and 5, points given for the plots
#' @param p_code numeric between 0 and 5, points given for the code
#' @param p_stat numeric between 0 and 5, points given for the
#'  statistic computations
#'
#' @details
#' The tables are created using [`knitr::kable()`] and [`kableExtra`] is
#' used for additional styling.
#'
#' `create_minreq_table()` creates a table that checks that the minimal requirements
#' are satisfied:
#' * the paper must be reproducible
#' * there must be at least on table and two kinds of plots
#' * there must be at least 5 plots and tables
#' * there must be at least two statistical computations
#'
#' The table lists for each of those requirement whether it is satisfied or not.
#'
#' `create_grading_table()` creates a table that gives grades in percent for
#' each of five categories:
#' * Text
#' * Tables
#' * Plots
#' * Code
#' * Statistical computations
#'
#' In each category, up to five points may be awarded. The last row of the
#' table gives the percentage over all categories.
#'
#' @return
#' both functions return an object of class `kableExtra`.
#'
#' @name grading_tables
#' @export

create_minreq_table <- function(repro, n_tab, n_plot_kinds, n_plots, n_stat) {

  yes_no <- function(l) c("Nein", "Ja")[l + 1]
  nok_ok <- function(l) c("NOK", "OK")[l + 1]

  # vector of minimal requirements
  min_req <- c(1, 1, 2, 5, 2)
  # titles of the requirements
  req_titles <- c("Reproduzierbarkeit", "Tabellen",
                  "Arten Plots", "Bilder/Tabellen",
                  "stat. Auswertungen")

  dplyr::tibble(
    Anforderung = paste(c("", "$\\geq 1$", "$\\geq 2$", "$\\geq 5$", "$\\geq 2$"),
                          req_titles),
    res = c(repro, n_tab, n_plot_kinds, n_tab + n_plots, n_stat),
    Beurteilung = c(nok_ok(.data$res[1]),
                    paste(.data$res[-1], req_titles[-1])),
    "Erf\u00fcllt" = yes_no(.data$res >= min_req)
  ) %>%
  dplyr::select(-"res") %>%
  kableExtra::kable(format = "html") %>%
  kableExtra::kable_styling()
}


#' @name grading_tables
#' @export

create_grading_table <- function(p_text, p_tab, p_plot, p_code, p_stat) {

  points <- c(p_text, p_tab, p_plot, p_code, p_stat)
  max <- c(5, 5, 5, 5, 5)

  if (any(points < 0 | points > max)) {
    stop("invalid points")
  }

  dplyr::tibble(
    Titel = c("Text", "Tabellen", "Plots", "Code", "Stat. Auswertungen"),
    Punkte = points,
    Von = max,
    Prozent = scales::percent(.data$Punkte / .data$Von)
  ) %>%
    dplyr::bind_rows(
      dplyr::tibble(Titel = "Total", Punkte = sum(.$Punkte),
                    Von = sum(.$Von),
                    Prozent = scales::percent(.data$Punkte / .data$Von))
    ) %>%
    kableExtra::kable(format = "html") %>%
    kableExtra::kable_styling() %>%
    kableExtra::row_spec(6, bold = TRUE, background = grDevices::gray(0.8))

}
