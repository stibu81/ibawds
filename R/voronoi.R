#' Create a Voronoi Diagram for a Clustering
#'
#' Create a Voronoi diagram for a given clustering object.
#'
#' @param cluster an object containing the result of a clustering, e.g.,
#'  created by [`kmeans()`]. It must contain the fields `cluster` and
#'  `centers`.
#' @param x,y character giving the names of the variables to be plotted
#'  on the x- and y-axis.
#' @param data The data that has been used to create the clustering. If this
#'  is provided, the extension of the plot is adapted to the data and the
#'  data points are plotted unless this is suppressed by specifying
#'  `show_data = FALSE`.
#' @param show_data should the data points be plotted? This is `TRUE` by default
#'  if `data` is given.
#' @param colour_data should the data points be coloured according to the
#'   assigned cluster?
#' @param legend should a colour legend for the clusters be plotted?
#' @param point_size numeric indicating the size of the data points and the
#'  cluster centres.
#' @param linewidth numeric indicating the width of the lines that separate
#'  the areas for the clusters. Set to 0 to show no lines at all.
#'
#' @details
#' The function uses the `deldir` package to create the polygons for the
#' Voronoi diagram. The code has been inspired by `ggvoronoi`, which can
#' handle more complex situations.
#'
#' @examples
#' \donttest{
#' cluster <- kmeans(iris[, 1:4], centers = 3)
#' voronoi_diagram(cluster, "Sepal.Length", "Sepal.Width", iris)
#' }
#'
#' @references
#' Garrett et al., *ggvoronoi: Voronoi Diagrams and Heatmaps with ggplot2*,
#' Journal of Open Source Software 3(32) (2018) 1096,
#' \doi{10.21105/joss.01096}
#'
#' @export

voronoi_diagram <- function(cluster, x, y, data = NULL,
                            show_data = !is.null(data),
                            colour_data = TRUE,
                            legend = TRUE,
                            point_size = 2,
                            linewidth = 0.7) {

  rlang::check_installed("deldir")

  # check that cluster contains the required fields
  if (any(!c("cluster", "centers") %in% names(cluster))) {
    stop("cluster must contain the fields 'cluster' and 'centers'.")
  }

  # get the cluster centers and ensure that the requested variables exist
  centers <- cluster$centers %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(.cluster = factor(1:dplyr::n()))
  if (!x %in% names(centers)) {
    stop("variable ", deparse(substitute(x)), " does not exist in ",
         deparse(substitute(cluster)), ".")
  }
  if (!y %in% names(centers)) {
    stop("variable ", deparse(substitute(y)), " does not exist in ",
         deparse(substitute(cluster)), ".")
  }

  # if data is provided, use it to create an bounding box for the diagram
  # deldir requires the edges to be given in order c(xmin, xmax, ymin, ymax)
  bbox <- if (!is.null(data)) {
    if (!x %in% names(data)) {
      stop("variable ", deparse(substitute(x)), " does not exist in ",
           deparse(substitute(data)), ".")
    }
    if (!y %in% names(data)) {
      stop("variable ", deparse(substitute(y)), " does not exist in ",
           deparse(substitute(data)), ".")
    }

    # add column with the cluster assignment
    data <- data %>%
      dplyr::mutate(.cluster = factor(cluster$cluster))

    # enlarge the region covered by the tiles based on the range of the data
    varrange <- function(var) {
      r <- range(var)
      r + c(-1, 1) * 0.05 * diff(r)
    }

    c(varrange(data[[x]]), varrange(data[[y]]))
  }

  # compute the polygons for the Voronoi diagram
  polys <- deldir::deldir(centers[[x]], centers[[y]], rw = bbox) %>%
    deldir::tile.list() %>%
    lapply(function(tile) {
      dplyr::as_tibble(tile[c("x", "y")])
    }) %>%
    dplyr::bind_rows(.id = ".cluster") %>%
    dplyr::rename(!!x := .data$x, !!y := .data$y) %>%
    dplyr::mutate(.cluster = stringr::str_remove(.data$.cluster, "^pt."))

  # create the Voronoi diagram without data points
  plot <- centers %>%
    ggplot2::ggplot(ggplot2::aes_string(x, y,
                                        fill = ".cluster",
                                        colour = ".cluster")) +
    ggplot2::geom_polygon(data = polys,
                          colour = "black", alpha = 0.8,
                          lwd = linewidth) +
    ggplot2::geom_point(shape = 18, size = 3 * point_size) +
    ggplot2::scale_fill_brewer(palette = "Pastel1", guide = "none") +
    ggplot2::scale_colour_brewer(palette = "Set1",
                                 guide = c("none", "legend")[legend + 1]) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .01)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = .01)) +
    ggplot2::labs(colour = "Cluster")

  # add the data points
  if (!is.null(data) && show_data) {
    plot <- if (colour_data) {
      plot + ggplot2::geom_point(data = data, size = point_size)
    } else {
      plot + ggplot2::geom_point(data = data, size = point_size,
                                 colour = "black")
    }
  }

  plot
}


#' Cluster Data According to Centres and Recompute Centres
#'
#' For a given dataset and given centres, `cluster_with_centers()`
#' assigns each data point to its closest centre and then recomputes
#' the centres as the mean of all points assigned to each class. An initial
#' set of random cluster centres can be obtained with `init_rand_centers()`.
#' These functions can be used to visualise the mechanism of k-means.
#'
#' @param data a data.frame containing only the variables to be used for
#'  clustering.
#' @param centers a data.frame giving the centres of the clusters. It must have
#'  the same number of columns as `data`.
#'
#' @return
#' a list containing two tibbles:
#' * `centers`: the new centres of the clusters computed after cluster assignment
#'     with the given centres
#' * `cluster`: the cluster assignment for each point in `data` using the
#'     centres that were passed to the function
#'
#' @examples
#' # demonstrate k-means with iris data
#' # keep the relevant columns
#' iris2 <- iris[, c("Sepal.Length", "Petal.Length")]
#'
#' # initialise the cluster centres
#' clust <- init_rand_centers(iris2, n = 3, seed = 2435)
#'
#' # plot the data with the cluster centres
#' library(ggplot2)
#' ggplot(iris2, aes(x = Sepal.Length, y = Petal.Length)) +
#'  geom_point(data = clust$centers, aes(colour = factor(1:3)),
#'             shape = 18, size = 6) +
#'  geom_point() +
#'  scale_colour_brewer(palette = "Set1")
#'
#' # assign clusters and compute new centres
#' clust_new <- cluster_with_centers(iris2, clust$centers)
#'
#' # plot the data with clustering
#' clust$cluster <- clust_new$cluster
#' voronoi_diagram(clust, x = "Sepal.Length", y = "Petal.Length",
#'                 data = iris2)
#'
#' # plot the data with new cluster centres
#' clust$centers <- clust_new$centers
#' voronoi_diagram(clust, x = "Sepal.Length", y = "Petal.Length",
#'                 data = iris2, colour_data = FALSE)
#'
#' # this procedure may be repeated until the algorithm converges
#'
#' @export

cluster_with_centers <- function(data, centers) {

  nc <- nrow(centers)
  n_vars <- ncol(data)
  if (ncol(centers) != n_vars) {
    stop("data and centers must have the same number of columns.")
  }

  dist_to_data <- function(i) {
    unlist(centers[i, ]) %>%
      rep(nrow(data)) %>%
      matrix(ncol = 2, byrow = TRUE) %>%
      magrittr::subtract(data) %>%
      magrittr::raise_to_power(2) %>%
      rowSums() %>%
      sqrt()
  }

  dists <- lapply(1:nc, dist_to_data) %>%
    magrittr::set_names(1:nc) %>%
    dplyr::as_tibble()

  clustered <- data %>%
    dplyr::mutate(cluster = factor(apply(dists, 1, which.min))  )

  centers <- clustered %>%
    dplyr::group_by(.data$cluster) %>%
    dplyr::summarise(dplyr::across(.fns = mean)) %>%
    dplyr::select(-"cluster")

  list(centers = centers, cluster = clustered$cluster)
}


#' @rdname cluster_with_centers
#'
#' @param n the number of cluster centres to create
#' @param seed a random seed for reproducibility
#'
#' @export

init_rand_centers <- function(data, n, seed = sample(1000:9999, 1)) {

  if (n < 2) {
    stop("n must be at least 2")
  }
  n <- round(n)

  set.seed(seed)

  centers <- data %>% dplyr::summarise(
    dplyr::across(
      .fns = function(x) stats::runif(n, min(x), max(x))
    )
  )

  list(
    centers = dplyr::as_tibble(centers),
    cluster = factor(sample(1:n, nrow(data), replace = TRUE))
  )
}
