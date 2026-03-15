# Simulate Throws With One Or More Fair Dice

Simulate throws with one or multiple fair dice with an arbitrary number
of faces.

## Usage

``` r
throw_dice(n, faces = 6L, dice = 1L)
```

## Arguments

- n:

  number of throws. The value is cast to integer.

- faces:

  the number of faces of the dice. The value is cast to integer.

- dice:

  the number of dices to use for each throw. The value is cast to
  integer.

## Value

an integer vector of length `n` with the results of the throws.

## Examples

``` r
# throw a single 6-sided dice 5 times
throw_dice(5)
#> [1] 6 1 6 4 2

# throw a single 20-sided dice 7 times
throw_dice(7, faces = 20)
#> [1] 19 12 13 16  7 12  3

# throw two 6-sided dice 9 times
throw_dice(9, dice = 2)
#> [1]  5  8  8  7  7  4 10 11  7
```
