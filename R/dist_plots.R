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
#'  function should be marked with a red dot (continuous) or a red bar
#'  (discrete).
#' @param var character giving the name of the quantile
#'  variable. This is only used to label the axes.
#' @param title character giving the title of the plot
#' @param is_discrete logical indicating whether this is a discrete distribution.
#'  For discrete distributions, a bar plot is created. If omitted, the function
#'  tries to automatically determine, whether the distributions is discrete.
#'  In case this should fail, set this argument explicitly.
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
                              title = "Verteilungsfunktion",
                              is_discrete = NULL) {

  # if is_discrete is not specified, try to auto-detect wheter this is a
  # discrete distribution
  if (is.null(is_discrete)) {
    is_discrete <- is_discrete_distribution(fun, ..., at = mean(range))
  }
  if (is_discrete) {
    range <- round(range)
    if (!is.null(points)) points <- round(points)
  }

  pfun <- function(x) fun(x, ...)

  # discrete plot #####
  if (is_discrete) {
    discrete <- dplyr::tibble(x = seq(min(range), max(range)),
                              F = pfun(.data$x),
                              marked = FALSE)

    if (!is.null(points)) {
      discrete$marked[discrete$x %in% points] <- TRUE
    }

    plot <- discrete %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$F, fill = .data$marked)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey35"),
                                 guide = "none") +
      ggplot2::scale_x_continuous(breaks = discrete$x, minor_breaks = NULL)

    if (!is.null(points)) {
      for (px in points) {
        py <- pfun(px)
        plot <- plot +
          ggplot2::annotate(
            "line",
            # subtract 0.45 to avoid changing the extent of the x-axis
            x = c(min(range) - 0.45, px), y = c(py, py),
            alpha = 0.6, linetype = "dashed",
            colour = "red", linewidth = 0.7
          )
      }
    }

  # continuous plot #####
  } else {
    plot <- dplyr::tibble(x = range) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$x)) +
      ggplot2::stat_function(fun = pfun, linewidth = 0.8) +
      ggplot2::scale_x_continuous(breaks = seq(min(range), max(range), by = 1))

    if (!is.null(points)) {
      for (px in points) {
        py <- pfun(px)
        plot <- plot +
          ggplot2::annotate("line",
                            x = c(min(range), px, px), y = c(py, py, 0),
                            alpha = 0.6, linetype = "dashed",
                            colour = "red", linewidth = 0.7) +
          ggplot2::annotate("point", x = px, y = py,
                            colour = "red", size = 3)
      }
    }
  }

  plot +
    ggplot2::labs(title = title,
                  x = var,
                  y = paste0("F(", var, ")"))
}

#' @rdname distribution_plot
#' @param from,to numeric values giving start and end of a
#'  range where the area under the density will be shaded (continuous)
#'  or the bars will be drawn in red (discrete).
#'  If only one of the two values is given, the shading
#'  will start at negative infinity or go until positive infinity,
#'  respectively.
#'
#' @export

density_plot <- function(fun, range, ...,
                         from = NULL, to = NULL,
                         points = NULL,
                         var = "x",
                         title = "Dichte",
                         is_discrete = NULL) {

  # if is_discrete is not specified, try to auto-detect wheter this is a
  # discrete distribution
  if (is.null(is_discrete)) {
    is_discrete <- is_discrete_distribution(fun, ..., at = mean(range))
  }
  if (is_discrete) {
    range <- round(range)
    if (!is.null(from)) from <- round(from)
    if (!is.null(to)) to <- round(to)
    if (!is.null(points)) points <- round(points)
  }

  dfun <- function(x) fun(x, ...)

  draw_area <- !is.null(from) || !is.null(to)

  if (draw_area) {
    if (is.null(from)) from <- min(range)
    if (is.null(to)) to <- max(range)
  }

  # discrete plot #####
  if (is_discrete) {
    discrete <- dplyr::tibble(x = seq(min(range), max(range)),
                              f = dfun(.data$x),
                              marked = FALSE)

    if (draw_area) {
      discrete$marked[dplyr::between(discrete$x, from, to)] <- TRUE
    }
    if (!is.null(points)) {
      discrete$marked[discrete$x %in% points] <- TRUE
    }

    plot <- discrete %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$f, fill = .data$marked)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey35"),
                                 guide = "none") +
      ggplot2::scale_x_continuous(breaks = discrete$x, minor_breaks = NULL)

  # continuous plot #####
  } else {
    plot <- dplyr::tibble(x = range) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$x)) +
      ggplot2::stat_function(fun = dfun, linewidth = 0.8) +
      ggplot2::scale_x_continuous(breaks = seq(min(range), max(range), by = 1))

    # add area, if requestd
    if (draw_area) {
      # determine number of points to use (at least 2)
      n_points <- max((to - from)/diff(range) * 200, 2)
      x_area <- seq(from, to, length.out = 50)
      plot <- plot +
        ggplot2::annotate("area", x = x_area, y = dfun(x_area),
                          fill = "red", alpha = 0.3)
    }

    # add points, if requested
    if (!is.null(points)) {
      for (px in points) {
        py <- dfun(px)
        plot <- plot +
          ggplot2::annotate("line",
                            x = c(min(range), px, px), y = c(py, py, 0),
                            alpha = 0.6, linetype = "dashed",
                            colour = "red", linewidth = 0.7) +
          ggplot2::annotate("point", x = px, y = py,
                            colour = "red", size = 3)
      }
    }
  }

  plot +
    ggplot2::labs(title = title,
                  x = var,
                  y = paste0("f(", var, ")"))
}


# Automatically detect, whether a distribution is discrete
# This takes advantage of the fact that the inbuilt discrete distributions in R
# all issue the warning "non-integer x" if called with non-integer numbers.

is_discrete_distribution <- function(fun, ..., at = 1.5) {

  # round of at to the nearest integer. The necessary non integer parts
  # will be added below
  x <- floor(at)

  warn <- tryCatch(fun(x + 0.5, ...), warning = function(w) w$message)
  # if there was a warning, warn is a character. Check this and that the
  # warning indicates a discrete distribution
  is_discrete <- is.character(warn) && stringr::str_detect(warn, "non-integer x = ")

  # discrete distribution function can be called with non-integer numbers
  # in order to detect these, we check that the value for 1.1 is the same as
  # for 1.9
  if (!is_discrete) {
    is_discrete <- (fun(x + 0.9, ...) - fun(x + 0.1, ...)) == 0
  }

  is_discrete
}
