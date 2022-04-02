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
#' @param legend should a colour legend for the clusters be plotted?
#' @param point_size numeric indicating the size of the data points and the
#'  cluster centres.
#' @param linewidth numeric indicating the width of the lines that separate
#'  the areas for the clusters. Set to 0 to show no lines at all.
#'
#' @examples
#' \donttest{
#' cluster <- kmeans(iris[, 1:4], centers = 3)
#' voronoi_diagram(cluster, "Sepal.Length", "Sepal.Width", iris)
#' }
#'
#' @export

voronoi_diagram <- function(cluster, x, y, data = NULL,
                         show_data = !is.null(data),
                         legend = TRUE,
                         point_size = 2,
                         linewidth = 0.7) {

  rlang::check_installed("ggvoronoi")

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

  # if data is provided, use it to create an outline for the diagram
  outline <- if (!is.null(data)) {
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

    # geom_voronoi() does not accept a tibble
    data.frame(
      x = varrange(data[[x]])[c(1, 1, 2, 2)],
      y = varrange(data[[y]])[c(1, 2, 2, 1)]
    )
  }

  # create the Voronoi diagram without data points
  plot <- centers %>%
    ggplot2::ggplot(ggplot2::aes_string(x, y,
                                        fill = ".cluster",
                                        colour = ".cluster")) +
    ggvoronoi::geom_voronoi(colour = "black", alpha = 0.8,
                            lwd = linewidth,
                            outline = outline) +
    ggplot2::geom_point(shape = 18, size = 3 * point_size) +
    ggplot2::scale_fill_brewer(palette = "Pastel1", guide = "none") +
    ggplot2::scale_colour_brewer(palette = "Set1",
                                 guide = c("none", "legend")[legend + 1]) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .01)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = .01)) +
    ggplot2::labs(colour = "Cluster")

  # add the data points
  if (!is.null(data) && show_data) {
    plot <- plot +
      ggplot2::geom_point(data = data, size = point_size)
  }

  plot
}
