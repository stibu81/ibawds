test_that("check find_similar_colour() for Euclidean distance", {
  expect_equal(find_similar_colour("green3"), "green3")
  expect_equal(find_similar_colour("#56d4a7"), "aquamarine3")
  expect_equal(find_similar_colour(c(63, 135, 253)), "royalblue1")
  expect_equal(find_similar_colour(col2rgb("blue2")), "blue2")
})

test_that("check find_similar_colour() for Manhattan distance", {
  expect_equal(find_similar_colour("green3", distance = "manhattan"),
               "green3")
  expect_equal(find_similar_colour("#56d4a7", distance = "manhattan"),
               "aquamarine3")
  expect_equal(find_similar_colour(c(63, 135, 253), distance = "manhattan"),
               "royalblue1")
  expect_equal(find_similar_colour(col2rgb("blue2"), distance = "manhattan"),
               "blue2")
})

test_that("check output control for find_similar_colour()", {
  expect_message(find_similar_colour("green3", verbose = TRUE),
                 "most similar colour is \"green3\"") %>%
    # catch the remainder of the output without checking its contents
    expect_output()
  expect_silent(find_similar_colour("green3", verbose = FALSE))
})

