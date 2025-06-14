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
#' @param p_text numeric between 0 and 3, points given for the text
#' @param p_tab numeric between 0 and 3, points given for the tables
#' @param p_plot numeric between 0 and 5, points given for the plots
#' @param p_code numeric between 0 and 5, points given for the code
#' @param p_stat numeric between 0 and 5, points given for the
#'  statistic computations
#' @param lang language to use in the tables. Supported languages are
#'  German (`"de"`, the default) and English ("`en"`).
#'
#' @details
#' The tables are created using [`knitr::kable()`] and [`kableExtra::kableExtra`] is
#' used for additional styling.
#'
#' `create_minreq_table()` creates a table that checks that the minimal requirements
#' are satisfied:
#' * the paper must be reproducible
#' * there must be at least one formatted table
#' * there must be at least 5 plots of at least three different types
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

create_minreq_table <- function(repro, n_tab, n_plot_kinds, n_plots, n_stat,
                                lang = c("de", "en")) {

  rlang::check_installed("kableExtra")

  lang <- rlang::arg_match(lang)

  yes_no <- function(l) tab_labs(c("Nein", "Ja"), lang, TRUE)[l + 1]
  nok_ok <- function(l) c("NOK", "OK")[l + 1]

  # don't accept negative points
  if (any(c(n_tab, n_plot_kinds, n_plots, n_stat) < 0)) {
    cli::cli_abort("invalid input: positive numbers expected.")
  }

  # vector of minimal requirements
  min_req <- c(1, 1, 3, 5, 2)
  # titles of the requirements
  req_titles <- c(
    tab_labs("Reproduzierbarkeit", lang, TRUE),
    tab_labs(c("Tabellen", "Arten Plots", "Plots", "stat. Auswertungen"), lang)
  )

  dplyr::tibble(
    requirement = paste(c("", "\u2265", "\u2265", "\u2265", "\u2265"),
                        c("", min_req[-1]),
                        req_titles),
    res = c(repro, n_tab, n_plot_kinds, n_plots, n_stat),
    assessment = c(nok_ok(.data$res[1]),
                   paste(.data$res[-1], req_titles[-1])),
    # setting the name with umlaut here leadds to a CRAN warning on Windows
    # => use no umlaut here, rename below
    satisfied = yes_no(.data$res >= min_req)
  ) %>%
  dplyr::select(-"res") %>%
  magrittr::set_names(
    tab_labs(c("Anforderung", "Beurteilung", "Erf\u00fcllt"), lang, TRUE)
  ) %>%
  kableExtra::kable(format = "html") %>%
  kableExtra::kable_styling()
}


#' @name grading_tables
#' @export

create_grading_table <- function(p_text, p_tab, p_plot, p_code, p_stat,
                                 lang = c("de", "en")) {

  rlang::check_installed("kableExtra")

  lang <- rlang::arg_match(lang)

  points <- c(p_text, p_tab, p_plot, p_code, p_stat)
  max <- c(3, 3, 5, 5, 5)

  if (any(points < 0 | points > max)) {
    cli::cli_abort("invalid points")
  }

  dplyr::tibble(
      title = tab_labs(
        c("Text", "Tabellen", "Plots", "Code", "stat. Auswertungen"),
        lang = lang,
        cap = TRUE
      ),
      points = points,
      max = max
    ) %>%
    dplyr::bind_rows(
      dplyr::tibble(
        title = tab_labs("Total", lang, TRUE),
        points = sum(.$points),
        max = sum(.$max)
      )
    ) %>%
    dplyr::mutate(
      perc = scales::label_percent(accuracy = 1)(.data$points / .data$max)) %>%
    purrr::set_names(
      tab_labs(c("Titel", "Punkte", "Von", "Prozent"), lang, TRUE)
    ) %>%
    kableExtra::kable(format = "html") %>%
    kableExtra::kable_styling() %>%
    kableExtra::row_spec(6, bold = TRUE, background = grDevices::gray(0.8))

}


# define the text labels in German and English
table_texts <- tibble::tribble(
                   ~de,                  ~en,
                "Text",               "text",
            "Tabellen",             "tables",
               "Plots",              "plots",
                "Code",               "code",
  "stat. Auswertungen", "stat. computations",
               "Total",              "total",
  "Reproduzierbarkeit",    "reproducibility",
         "Arten Plots",     "kinds of plots",
         "Anforderung",        "requirement",
         "Beurteilung",         "assessment",
        "Erf\u00fcllt",          "satisfied",
               "Titel",              "title",
              "Punkte",             "points",
                 "Von",                "max",
             "Prozent",         "percentage",
                  "Ja",                "yes",
                "Nein",                 "no"
  )


tab_labs <- function(texts, lang, cap = FALSE,
                     error_call = rlang::caller_env()) {

  # find the values in the table
  it <- match(texts, table_texts$de)

  # abort, if one of the texts is not found
  if (any(is.na(it))) {
    cli::cli_abort("invalid text(s) requested: {texts[is.na(it)]}",
                   call = error_call)
  }

  labs <- table_texts[[lang]][it]

  if (cap) labs <- stringr::str_to_sentence(labs, locale = lang)

  labs
}
