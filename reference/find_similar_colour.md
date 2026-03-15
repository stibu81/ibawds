# Find a Named Colour that is Similar to Any Given Colour

Find the named colour that is most similar to a given colour.

## Usage

``` r
find_similar_colour(
  colour,
  distance = c("euclidean", "manhattan"),
  verbose = interactive()
)
```

## Arguments

- colour:

  a colour specified in one of three forms: a hexadecimal string of the
  form `"#rrggbb"` or `"#rrggbbaa"`, a numeric vector of length 3 or a
  numeric matrix with dimensions `c(3, 1)`, as it is returned by
  [`col2rgb()`](https://rdrr.io/r/grDevices/col2rgb.html). Numeric
  values must be between 0 and 255.

- distance:

  character indicating the distance metric to be used.

- verbose:

  should additional output be produced? This shows the RGB values for
  the input colour, the most similar named colour and the difference
  between the two.

## Value

a character of length one with the name of the most similar named
colour.

## Examples

``` r
find_similar_colour("#d339da")
#> [1] "mediumorchid"
find_similar_colour(c(124, 34, 201))
#> [1] "purple3"

# suppress additional output
find_similar_colour("#85d3a1", verbose = FALSE)
#> [1] "darkseagreen3"

# use Manhattan distance
find_similar_colour(c(124, 34, 201), distance = "manhattan")
#> [1] "purple3"
```
