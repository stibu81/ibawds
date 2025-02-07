test_that("throw_dice() with one dice works", {
  expect_type(throw_dice(3), "integer")
  expect_length(throw_dice(57), 57)
  set.seed(2345)
  expect_setequal(throw_dice(200), 1:6)
  expect_setequal(throw_dice(400, 20), 1:20)
})


test_that("throw_dice() with two dice works", {
  expect_type(throw_dice(3, dice = 2), "integer")
  expect_length(throw_dice(57, dice = 2), 57)
  set.seed(2345)
  expect_setequal(throw_dice(1000, dice = 2), 2:12)
  expect_setequal(throw_dice(5000, 20, dice = 2), 2:40)
})


test_that("throw_dice() with five dice works", {
  expect_type(throw_dice(3, dice = 5), "integer")
  expect_length(throw_dice(57, dice = 5), 57)
  set.seed(2345)
  expect_setequal(throw_dice(10000, 4, dice = 5), 5:20)
})


test_that("throw_dice() results have the expected distribution", {
  set.seed(6756)
  expect_gt(
    chisq.test(table(throw_dice(200)), p = rep(1/6, 6))$p.value,
    0.05
  )
  expect_gt(
    chisq.test(table(throw_dice(500, 20)), p = rep(1/20, 20))$p.value,
    0.05
  )
  expect_gt(
    chisq.test(
      table(throw_dice(1000, dice = 2)),
      p = c(1:6, 5:1) / 36
    )$p.value,
    0.05
  )
})

test_that("throw_dice() throws errors", {
  expect_error(throw_dice(-3), "n must be at least 1")
  expect_error(throw_dice(5, 1), "faces must be at least 2")
  expect_error(throw_dice(5, dice = 0), "dice must be at least 1")
})
