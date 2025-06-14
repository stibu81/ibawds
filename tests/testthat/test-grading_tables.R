library(rvest)
library(dplyr, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)

# reference tables in German
mrt1_ref_de <- tibble(
  Anforderung = c("Reproduzierbarkeit", "\u2265 1 Tabellen",
                  "\u2265 3 Arten Plots", "\u2265 5 Plots",
                  "\u2265 2 stat. Auswertungen"),
  Beurteilung = c("OK", "3 Tabellen", "3 Arten Plots", "5 Plots",
                  "2 stat. Auswertungen"),
  "Erf\u00fcllt" = "Ja"
)
mrt2_ref_de <- mrt1_ref_de %>%
  mutate(
    Beurteilung = c("NOK", "2 Tabellen", "3 Arten Plots", "3 Plots",
                    "0 stat. Auswertungen"),
    "Erf\u00fcllt" = c("Nein", "Ja", "Ja", "Nein", "Nein")
)

grt1_ref_de <- tibble(
  Titel = c("Text", "Tabellen", "Plots", "Code", "Stat. Auswertungen", "Total"),
  Punkte = c(2.5, 2, 3, 1, 2, 10.5),
  Von = c(3L, 3L, 5L, 5L, 5L, 21L),
  Prozent = c("83%", "67%", "60%", "20%", "40%", "50%")
)

# reference tables in English

mrt2_ref_en <- tibble(
  Requirement = c("Reproducibility", "\u2265 1 tables",
                  "\u2265 3 kinds of plots", "\u2265 5 plots",
                  "\u2265 2 stat. computations"),
  Assessment = c("NOK", "2 tables", "3 kinds of plots", "3 plots",
                 "0 stat. computations"),
  Satisfied = c("No", "Yes", "Yes", "No", "No")
)

grt1_ref_en <- grt1_ref_de %>%
  mutate(
    Titel = c("Text", "Tables", "Plots", "Code", "Stat. computations", "Total")
  ) %>%
  set_names(c("Title", "Points", "Max", "Percentage"))

# helper function to parse the tables
parse_tab <- function(x) {
    as.character(x) %>%
    read_html() %>%
    html_table() %>%
    extract2(1)
}

test_that("test create_minreq_table", {
  mrt1 <- create_minreq_table(TRUE, 3, 3, 5, 2)
  expect_s3_class(mrt1, "kableExtra")
  expect_equal(parse_tab(mrt1), mrt1_ref_de)

  mrt2 <- create_minreq_table(FALSE, 2, 3, 3, 0)
  expect_s3_class(mrt2, "kableExtra")
  expect_equal(parse_tab(mrt2), mrt2_ref_de)
})


test_that("test create_grading_table", {
  grt1 <- create_grading_table(2.5, 2, 3, 1, 2)
  expect_s3_class(grt1, "kableExtra")
  expect_equal(parse_tab(grt1), grt1_ref_de)
})

test_that("test English tables", {
  mrt2 <- create_minreq_table(FALSE, 2, 3, 3, 0, lang = "en")
  expect_s3_class(mrt2, "kableExtra")
  expect_equal(parse_tab(mrt2), mrt2_ref_en)

  grt1 <- create_grading_table(2.5, 2, 3, 1, 2, lang = "en")
  expect_s3_class(grt1, "kableExtra")
  expect_equal(parse_tab(grt1), grt1_ref_en)
})


test_that("test invalid inputs", {
  expect_error(create_grading_table(1, 1, 1, 1, -1),
               "invalid points")
  expect_error(create_grading_table(1, 1, 6, 1, 1),
               "invalid points")
  expect_error(create_minreq_table(TRUE, 1, 1, 1, -1),
               "invalid input")
  expect_error(create_minreq_table(TRUE, 1, -1, 1, 1),
               "invalid input")
  expect_error(create_minreq_table(TRUE, 1, 1, 1, 1, lang = "xy"),
               "`lang` must be one of")
})


test_that("tab_labs() works", {
  expect_equal(tab_labs(c("Tabellen", "stat. Auswertungen"), lang = "de"),
               c("Tabellen", "stat. Auswertungen"))
  expect_equal(tab_labs(c("Tabellen", "stat. Auswertungen"), lang = "en"),
               c("tables", "stat. computations"))

  expect_equal(
    tab_labs(c("Tabellen", "stat. Auswertungen"), lang = "de", cap = TRUE),
    c("Tabellen", "Stat. Auswertungen")
  )
  expect_equal(
    tab_labs(c("Tabellen", "stat. Auswertungen"), lang = "en", cap = TRUE),
    c("Tables", "Stat. computations")
  )
})


test_that("tab_labs() handles invalid input", {
  expect_null(tab_labs(c("Tabellen", "stat. Auswertungen"), lang = "fr"))
  expect_error(tab_labs(c("abc", "Tabellen", "def")),
               "invalid text\\(s\\) requested: abc and def")
})
