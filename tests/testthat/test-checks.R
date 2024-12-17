test_that("spell_check_slides() works", {
  spell_check_ref <- data.frame(word = c("Bird", "Schreibfehlr", "Wordlist"))
  spell_check_ref$found <- list("test.Rmd:14", "test.Rmd:20", "test.Rmd:22")
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
  withr::with_tempfile("rmd_file", {
    writeLines(c("<!-- nospellcheck -->", "", "something"), rmd_file)
    expect_true(is_no_spell_check(rmd_file))
    fileext = ".Rmd"
  })
})


test_that("check_url() works", {
  expect_true(check_url("https://www.github.com"))
  expect_false(check_url("https://thispagedoesnotexistonthe.internet"))
})
