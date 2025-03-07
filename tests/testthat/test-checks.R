library(dplyr, warn.conflicts = FALSE)

test_that("spell_check_slides() works", {
  skip_on_os(c("mac", "windows"))
  skip_on_cran()
  spell_check_ref <- data.frame(word = c("Bird", "Schreibfehlr", "Wordlist"))
  spell_check_ref$found <- list(
    c("test.Rmd:14", "test.qmd:14"),
    c("test.Rmd:22", "test.qmd:22"),
    c("test.Rmd:24", "test.qmd:24")
  )
  class(spell_check_ref) <- c("summary_spellcheck", "data.frame")

  expect_equal(spell_check_slides(test_path("data")), spell_check_ref)

  # not using the wordlist leads to more spelling errors
  expect_gt(
    nrow(spell_check_slides(test_path("data"), use_wordlist = FALSE)),
    nrow(spell_check_ref)
  )

  # check empty directory
  withr::with_tempdir({
    expect_error(spell_check_slides(), "No RMarkdown files found.")
  })
})


test_that("is_no_spell_check() works", {
  expect_false(is_no_spell_check(test_path("data", "01_Rmd", "test.Rmd")))
  expect_false(is_no_spell_check(test_path("data", "01_Rmd", "test.qmd")))

  withr::with_tempfile("rmd_file", {
    writeLines(c("<!-- nospellcheck -->", "", "something"), rmd_file)
    expect_true(is_no_spell_check(rmd_file))
    fileext = ".Rmd"
  })
  withr::with_tempfile("qmd_file", {
    writeLines(c("<!-- nospellcheck -->", "", "something"), qmd_file)
    expect_true(is_no_spell_check(qmd_file))
    fileext = ".qmd"
  })
})


test_that("spell_check_evaluation() works", {
  skip_on_os(c("mac", "windows"))
  skip_on_cran()
  spell_check_ref <- data.frame(word = c("Schreibfehlr", "Wordlist"))
  spell_check_ref$found <- list(
    c("Beurteilung_Reto.Rmd:21", "Beurteilung_Sandro.qmd:21"),
    c("Beurteilung_Reto.Rmd:12", "Beurteilung_Sandro.qmd:12")
  )
  class(spell_check_ref) <- c("summary_spellcheck", "data.frame")

  expect_equal(spell_check_evaluation(test_path("data")), spell_check_ref)

  # not using the wordlist leads to more spelling errors
  expect_gt(
    nrow(spell_check_evaluation(test_path("data"), use_wordlist = FALSE)),
    nrow(spell_check_ref)
  )

  # check with student filter
  spell_check_ref <- data.frame(word = c("Sandro", "Schreibfehlr", "Wordlist"))
  spell_check_ref$found <- list("Beurteilung_Reto.Rmd:10",
                                "Beurteilung_Reto.Rmd:21",
                                "Beurteilung_Reto.Rmd:12")
  class(spell_check_ref) <- c("summary_spellcheck", "data.frame")
  expect_equal(spell_check_evaluation(test_path("data"), students = "Reto"),
               spell_check_ref)
  expect_error(spell_check_evaluation(test_path("data"), students = "John"),
               "No evaluation files found.")

  # check empty directory
  withr::with_tempdir({
    expect_error(spell_check_evaluation(), "No evaluation files found.")
  })
})


test_that("check_url() works", {
  skip_on_cran()
  expect_true(check_url("https://www.github.com"))
  expect_false(check_url("https://thispagedoesnotexistonthe.internet"))
  # status code 204 (no content) and 301 (moved permanently) should be success
  expect_true(check_url("https://httpstat.us/201"))
  expect_true(check_url("https://httpstat.us/301"))
  # status codes 403 (forbidden) and 500 (int. server error) should be failure
  expect_false(check_url("https://httpstat.us/403"))
  expect_false(check_url("https://httpstat.us/500"))
  # an inexistent path on an existing page should be failure
  expect_false(check_url("https://www.github.com/stibu81/thisdoesnotexist"))
})


test_that("check_links_in_file() works", {
  skip_on_cran()
  link_check_ref <- tibble(
    url = c("https://de.wikipedia.org/wiki/Wikipedia:Hauptseite",
            "http://www.github.com",
            "https://en.wikipedia.org/wiki/Bird_(1988_film)",
            "https://en.wikipedia.org/wiki/Physics_(Aristotle)",
            "https://en.wikipedia.org/wiki/The_Treachery_of_Images",
            "https://www.doesnotexist.invalid"),
    reachable = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_equal(
    check_links_in_file(test_path("data", "01_Rmd", "test.Rmd")),
    link_check_ref
  )
  expect_equal(
    check_links_in_file(test_path("data", "01_Rmd", "test.qmd")),
    link_check_ref
  )

  # check a file without links
  withr::with_tempfile("rmd_file", {
    writeLines(c("there", "are no", "links"), rmd_file)
    expect_equal(check_links_in_file(rmd_file),
                 tibble(url = character(0), reachable = logical(0)))
    fileext = ".Rmd"
  })
})


test_that("extract_urls() works", {
  expect_equal(
    extract_urls(
      c("https://www.github.com", "{http://www.github.com}#",
        "www.github.com, (https://www.github.com)",
        "no url", "https:/invalid.url",
        "(https://www.github.com) %4l& [http://www.github.com] ",
        "abc https://www.ibaw.ch/suche/?q=data+science abc",
        # check handling of brackets
        "(this: https://en.wikipedia.org/wiki/Data_science#Modern_usage)",
        "Bird: https://en.wikipedia.org/wiki/Bird_(1988_film)",
        "Lorem ipsum dolor sit amet",
        "Bird: (https://en.wikipedia.org/wiki/Bird_(1988_film))",
        "(Bird: (https://en.wikipedia.org/wiki/Bird_((1988_film))))",
        "Bird: (https://en.wikipedia.org/wiki/Bird_(1988_film)):",
        "(Bird: (https://en.wikipedia.org/wiki/Bird_((1988_film)))):",
        "Bird: (https://en.wikipedia.org/wiki/Bird_(1988_film)).",
        "(Bird: (https://en.wikipedia.org/wiki/Bird_((1988_film)))).",
        "((https://en.wikipedia.org/wiki/Physics_(Aristotle)#Description_of_the_content):.&):-)",
        "https://en.wikipedia.org/w/index.php?search=Bird&title=Special%3ASearch&ns0=1",
        "sfd https://de.wikipedia.org/wiki/Wikipedia:Hauptseite =3",
        # check urls followed by punctuation
        "https://www.github.com.", "https://www.github.com:",
        "https://www.github.com,", "https://www.github.com;",
        "https://www.github.com?", "https://www.github.com!")
    ),
    c("https://www.github.com", "http://www.github.com",
      "https://www.github.com",
      "https://www.github.com", "http://www.github.com",
      "https://www.ibaw.ch/suche/?q=data+science",
      "https://en.wikipedia.org/wiki/Data_science#Modern_usage",
      "https://en.wikipedia.org/wiki/Bird_(1988_film)",
      rep(c("https://en.wikipedia.org/wiki/Bird_(1988_film)",
            "https://en.wikipedia.org/wiki/Bird_((1988_film))"),
          times = 3),
      "https://en.wikipedia.org/wiki/Physics_(Aristotle)#Description_of_the_content",
      "https://en.wikipedia.org/w/index.php?search=Bird&title=Special%3ASearch&ns0=1",
      "https://de.wikipedia.org/wiki/Wikipedia:Hauptseite",
      rep("https://www.github.com", times = 6)
    )
  )
})


test_that("check_links_in_slides() works", {
  skip_on_os("windows")
  skip_on_cran()
  expect_equal(
    # suppress the progress bar
    suppressMessages(check_links_in_slides(test_path("data"))),
    tibble(url = "https://www.doesnotexist.invalid",
           file = c("test.Rmd", "test.qmd"))
  )
})
