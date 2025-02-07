#' Simulate Throws With One Or More Fair Dice
#'
#' Simulate throws with one or multiple fair dice with an arbitrary
#' number of faces.
#'
#' @param n number of throws. The value is cast to integer.
#' @param faces the number of faces of the dice. The value is cast to integer.
#' @param dice the number of dices to use for each throw. The value is
#'  cast to integer.
#'
#' @returns
#' an integer vector of length `n` with the results of the throws.
#'
#' @examples
#' # throw a single 6-sided dice 5 times
#' throw_dice(5)
#'
#' # throw a single 20-sided dice 7 times
#' throw_dice(7, faces = 20)
#'
#' # throw two 6-sided dice 9 times
#' throw_dice(9, dice = 2)
#'
#' @export

throw_dice <- function(n, faces = 6L, dice = 1L) {

  # make sure, n and faces are integers
  n <- as.integer(n)
  faces <- as.integer(faces)
  dice <- as.integer(dice)

  # check inputs
  if (n < 1) {
    cli::cli_abort("n must be at least 1")
  }
  if (faces < 2) {
    cli::cli_abort("faces must be at least 2")
  }
  if (dice < 1) {
    cli::cli_abort("dice must be at least 1")
  }

  sample(1:faces, n * dice, replace = TRUE) %>%
    matrix(ncol = n) %>%
    colSums() %>%
    as.integer()
}
