library(ggplot2)

test_that("test define_latex_stats()", {
  expect_output(
    expect_invisible(out <-define_latex_stats(), "\\\\newcommand\\{")
  )
  expect_null(out)
})

# store current options as reference
ref_opts <- list(ggplot = theme_get(),
                 base = options("pillar.print_max", "pillar.print_min"))

test_that("test set_slide_options() with default values", {
  on.exit({
    theme_set(ref_opts$ggplot)
    options(ref_opts$base)
  })
  expect_invisible(set_slide_options()) %>%
    expect_equal(ref_opts)
  expect_equal(theme_get()$text$size, 22)
  expect_equal(as.numeric(theme_get()$plot.margin), rep(10, 4))
  expect_equal(options("pillar.print_max", "pillar.print_min"),
               list(pillar.print_max = 12, pillar.print_min = 8))
})

test_that("test set_slide_options() with other values", {
  on.exit({
    theme_set(ref_opts$ggplot)
    options(ref_opts$base)
  })
  expect_equal(set_slide_options(ggplot_text_size = 18,
                                 ggplot_margin_pt = c(4, 6, 9, 2),
                                 tibble_print_max = 33,
                                 tibble_print_min = 2),
               ref_opts)
  expect_equal(theme_get()$text$size, 18)
  expect_equal(as.numeric(theme_get()$plot.margin), c(4, 6, 9, 2))
  expect_equal(options("pillar.print_max", "pillar.print_min"),
               list(pillar.print_max = 33, pillar.print_min = 2))
})
