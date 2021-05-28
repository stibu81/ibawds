#' Plot Density and Distribution Function With Markings
#'
#' Create plots of the density and distribution functions
#' of a probability distribution. It is possible to
#' mark points and shade the area under the curve.
#'
#' @param fun a density or distribution function that takes
#'  quantiles as its first argument.
#' @param range numeric vector of length two giving the
#'  range of quantiles to be plotted.
#' @param ... further arguments that are passed to `fun()`.
#' @param points numeric vector giving quantiles where the
#'  function should be marked with a red dot.
#' @param var character giving the name of the quantile
#'  variable. This is only used to label the axes.
#' @param title character giving the title of the plot
#'
#' @return
#' a \code{ggplot} object
#'
#' @examples
#' # plot density of the normal distribution
#' density_plot(dnorm, c(-5, 7),
#'              mean = 1, sd = 2,
#'              to = 3)
#'
#' # plot distribution function of the Poisson distribution
#' distribution_plot(ppois, c(0, 12),
#'                   lambda = 4,
#'                   points = c(2, 6, 10),
#'                   var = "y")
#'
#' @export

distribution_plot <- function(fun, range, ...,
                              points = NULL,
                              var = "x",
                              title = "Verteilungsfunktion") {

  pfun <- function(x) fun(x, ...)

  plot <- ggplot(data.frame(x = range), aes_(x = ~x)) +
            stat_function(fun = pfun, size = 0.8) +
            scale_x_continuous(breaks = seq(min(range), max(range), by = 1)) +
            labs(title = title,
                 x = var,
                 y = paste0("F(", var, ")"))

  if (!is.null(points)) {
    for (px in points) {
      py <- pfun(px)
      plot <- plot +
        annotate("line", x = c(min(range), px, px), y = c(py, py, 0),
                 alpha = 0.6, linetype = "dashed", colour = "red", size = 0.7) +
        annotate("point", x = px, y = py, colour = "red", size = 3)
    }
  }

  return(plot)
}

#' @rdname distribution_plot
#' @param from,to numeric values giving start and end of a
#'  range where the area under the density will be shaded.
#'  If only one of the two values is given, the shading
#'  will start a negative infinity or go until positive infinity,
#'  respectively.
#'
#' @export

density_plot <- function(fun, range, ...,
                         from = NULL, to = NULL,
                         points = NULL,
                         var = "x",
                         title = "Dichte") {

  dfun <- function(x) fun(x, ...)

  draw_area <- !is.null(from) || !is.null(to)

  if (draw_area) {
    if (is.null(from)) from <- min(range)
    if (is.null(to)) to <- max(range)
  }

  plot <- ggplot(data.frame(x = range), aes_(x = ~x)) +
            stat_function(fun = dfun, size = 0.8) +
            scale_x_continuous(breaks = seq(min(range), max(range), by = 1)) +
            labs(title = title,
                 x = var,
                 y = paste0("f(", var, ")"))

  # add area, if requestd
  if (draw_area) {
    # determine number of points to use (at least 2)
    n_points <- max((to - from)/diff(range) * 200, 2)
    x_area <- seq(from, to, length.out = 50)
    plot <- plot +
              annotate("area", x = x_area, y = dfun(x_area),
                       fill = "red", alpha = 0.3)
  }

  # add points, if requested
  if (!is.null(points)) {
     for (px in points) {
      py <- dfun(px)
      plot <- plot +
        annotate("line", x = c(min(range), px, px), y = c(py, py, 0),
                 alpha = 0.6, linetype = "dashed", colour = "red", size = 0.7) +
        annotate("point", x = px, y = py, colour = "red", size = 3)
    }
  }

  return(plot)
}
