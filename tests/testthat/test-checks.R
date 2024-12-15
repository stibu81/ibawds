test_that("check_url() works", {
  expect_true(check_url("https://www.github.com"))
  expect_false(check_url("https://thispagedoesnotexistonthe.internet"))
})
