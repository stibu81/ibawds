# Rescale Mean And/Or Standard Deviation of a Vector

Rescale Mean And/Or Standard Deviation of a Vector

## Usage

``` r
rescale(x, mu = mean(x), sigma = sd(x))
```

## Arguments

- x:

  numeric vector

- mu:

  numeric value giving the desired mean

- sigma:

  numeric value giving the desired standard deviation

## Value

a numeric vector with the same length as `x` with mean `mu` and standard
deviation `sigma`.

## Details

By default, mean and standard deviation are not changed, i.e.,
`rescale(x)` is identical to `x`. Only if a value is specified for `mu`
and/or `sigma` the mean and/or the standard deviation are rescaled.

## Examples

``` r
x <- runif(1000, 5, 8)

# calling rescale without specifying mu and sigma doesn't change anything
all.equal(x, rescale(x))
#> [1] TRUE

# change the mean without changing the standard deviation
x1 <- rescale(x, mu = 3)
all.equal(mean(x1), 3)
#> [1] TRUE
all.equal(sd(x1), sd(x))
#> [1] TRUE

# rescale mean and standard deviation
x2 <- rescale(x, mu = 3, sigma = 2)
all.equal(mean(x2), 3)
#> [1] TRUE
all.equal(sd(x2), 2)
#> [1] TRUE
```
