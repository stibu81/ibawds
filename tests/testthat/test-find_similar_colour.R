test_that("check find_similar_colour() for Euclidean distance", {
  expect_equal(find_similar_colour("green3", verbose = FALSE), "green3")
  expect_equal(find_similar_colour("#56d4a7", verbose = FALSE), "aquamarine3")
  expect_equal(find_similar_colour(c(63, 135, 253), verbose = FALSE), "royalblue1")
  expect_equal(find_similar_colour(col2rgb("blue2"), verbose = FALSE), "blue2")
})

test_that("check find_similar_colour() for Manhattan distance", {
  expect_equal(
    find_similar_colour("green3", distance = "manhattan", verbose = FALSE),
    "green3"
  )
  expect_equal(
    find_similar_colour("#56d4a7", distance = "manhattan", verbose = FALSE),
    "aquamarine3"
  )
  expect_equal(
    find_similar_colour(c(63, 135, 253), distance = "manhattan", verbose = FALSE),
    "royalblue1"
  )
  expect_equal(
    find_similar_colour(col2rgb("blue2"), distance = "manhattan", verbose = FALSE),
    "blue2"
  )
})

test_that("check output control for find_similar_colour()", {
  expect_message(find_similar_colour("green3", verbose = TRUE),
                 "most similar colour is \"green3\"") %>%
    # catch the remainder of the output without checking its contents
    expect_output()
  expect_silent(find_similar_colour("green3", verbose = FALSE))
})


test_that("check messages for find_similar_colour()", {
  expect_error(find_similar_colour(TRUE), "invalid input for colour")
  expect_error(find_similar_colour(c(1, 2)),
               "if colour is a numeric vector, it must have length 3")
  expect_error(find_similar_colour(c(1, 2, 256)),
               "if colour is a numeric vector, all values must be between 0 and 255")
  expect_error(find_similar_colour(c(1, -2, 120)),
               "if colour is a numeric vector, all values must be between 0 and 255")
  expect_error(find_similar_colour(matrix(c(1, 2), ncol = 1)),
               "if colour is a numeric matrix, it must have dimensions c\\(3, 1\\)")
  expect_error(find_similar_colour(matrix(c(1, 2, 3), ncol = 3)),
               "if colour is a numeric matrix, it must have dimensions c\\(3, 1\\)")
  expect_error(find_similar_colour(matrix(c(1, 2, 256), ncol = 1)),
               "if colour is a numeric matrix, all values must be between 0 and 255")
  expect_error(find_similar_colour(matrix(c(1, -2, 120), ncol = 1)),
               "if colour is a numeric matrix, all values must be between 0 and 255")
})
