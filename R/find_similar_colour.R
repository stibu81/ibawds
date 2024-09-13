#' Find a Named Colour that is Similar to Any Given Colour
#'
#' Find the named colour that is most similar to a given colour.
#'
#' @param colour a colour specified in one of three forms: a hexadecimal string
#'  of the form `"#rrggbb"` or `"#rrggbbaa"`, a numeric vector of length 3 or a
#'  numeric matrix with dimensions `c(3, 1)`, as it is returned by
#'  [`col2rgb()`].
#'  Numeric values must be between 0 and 255.
#' @param distance character indicating the distance metric to be used.
#' @param verbose should additional output be produced? This shows the RGB values
#'  for the input colour, the most similar named colour and the difference between
#'  the two.
#'
#' @return
#' a character of length one with the name of the most similar named colour.
#'
#' @examples
#' find_similar_colour("#d339da")
#' find_similar_colour(c(124, 34, 201))
#'
#' # suppress additional output
#' find_similar_colour("#85d3a1", verbose = FALSE)
#'
#' # use Manhattan distance
#' find_similar_colour(c(124, 34, 201), distance = "manhattan")
#'
#' @export

find_similar_colour <- function(colour,
                                distance = c("euclidean", "manhattan"),
                                verbose = interactive()) {

  distance <- match.arg(distance)

  col_rgb <-
    # if colour is a character, convert to RGB
    if (is.character(colour)) {
      grDevices::col2rgb(colour)
    # if it's a numeric vector, check and convert to a matrix
    } else if (is.numeric(colour) && is.vector(colour)) {
      if (length(colour) != 3) {
        cli::cli_abort("if colour is a numeric vector, it must have length 3")
      }
      if (any(colour > 255 | colour < 0)) {
        cli::cli_abort("if colour is a numeric vector, all values must be between 0 and 255.")
      }
      matrix(colour, ncol = 1, dimnames = list(c("red", "green", "blue")))
    # if it's a numeric matrix, check and return unchanged
    } else if (is.numeric(colour) && is.matrix(colour)) {
      if (!identical(dim(colour), c(3L, 1L))) {
        cli::cli_abort("if colour is a numeric matrix, it must have dimensions c(3, 1).")
      }
      if (any(colour > 255 | colour < 0)) {
        cli::cli_abort("if colour is a numeric matrix, all values must be between 0 and 255.")
      }
      colour
    # any other format is invalid
    } else {
      cli::cli_abort("invalid input for colour")
    }

  # prepare all the named colours in the right format
  cols <- grDevices::colours()
  cols_rgb <- grDevices::col2rgb(cols)

  # compute the distances and find the most similar colour
  i_min <- which_min_dist(col_rgb, cols_rgb, distance)
  sim_col <- cols[i_min]

  if (verbose) {
    cli::cli_alert_info(paste0("most similar colour is \"", sim_col, "\""))
    cbind(col_rgb, cols_rgb[, i_min], cols_rgb[, i_min] - col_rgb) %>%
      magrittr::set_colnames(c("input", sim_col, "difference")) %>%
      print()
    cat("\n")
  }

  sim_col

}


# compute the distance between x and each value of y

which_min_dist <- function(x, y, method) {
  dist_fun <- if (method == "euclidean") {
    function(z) sum((x - z)^2)
  } else {
    function(z) sum(abs(x - z))
  }
  apply(y, 2, dist_fun) %>%
    which.min()
}
