library(vdiffr)
library(dplyr, warn.conflicts = FALSE)

set.seed(3125)
clust <- kmeans(iris[, 1:4], centers = 3)

test_that("test voronoi_diagram()", {
  expect_doppelganger(
    "voronoi default",
    voronoi_diagram(clust, "Sepal.Width", "Sepal.Length")
  )
  expect_doppelganger(
    "voronoi with data",
    voronoi_diagram(clust, "Petal.Width", "Sepal.Length", data = iris)
  )
  expect_doppelganger(
    "voronoi suppress data",
    voronoi_diagram(clust, "Petal.Width", "Petal.Length",
                 data = iris, show_data = FALSE)
  )
  expect_doppelganger(
    "voronoi with options",
    voronoi_diagram(clust, "Sepal.Width", "Petal.Length",
                 data = iris, legend = FALSE,
                 point_size = 3, linewidth = 1.2)
  )
})

test_that("test voronoi_diagram() with invalid inputs", {
  expect_error(voronoi_diagram(iris, "Sepal.Width", "Sepal.Length"),
               "cluster must contain the fields 'cluster' and 'centers'.")
  expect_error(voronoi_diagram(clust, "Sepal.Width", "something"),
               "variable \"something\" does not exist in clust")
  expect_error(voronoi_diagram(clust, "something", "Sepal.Length"),
               "variable \"something\" does not exist in clust")
  iris_bad <- rename(iris, something = Sepal.Width)
  expect_error(
    voronoi_diagram(clust, "Sepal.Width", "Sepal.Length", data = iris_bad),
    "variable \"Sepal.Width\" does not exist in iris_bad"
  )
  expect_error(
    voronoi_diagram(clust, "Sepal.Length", "Sepal.Width", data = iris_bad),
    "variable \"Sepal.Width\" does not exist in iris_bad"
  )
})
