# Create a Random Vector With Fixed Correlation With Another Vector

`rand_with_cor()` creates a vector of random number that has correlation
`rho` with a given vector `y`. Also mean and standard deviation of the
random vector can be fixed by the user. By default, they will be equal
to the mean and standard deviation of `y`, respectively.

## Usage

``` r
rand_with_cor(y, rho, mu = mean(y), sigma = sd(y))
```

## Source

This solution is based on an
[answer](https://stats.stackexchange.com/a/313138/64220) by
[whuber](https://stats.stackexchange.com/users/919/whuber) on [Cross
Validated](https://stats.stackexchange.com).

## Arguments

- y:

  a numeric vector

- rho:

  numeric value between -1 and 1 giving the desired correlation.

- mu:

  numeric value giving the desired mean

- sigma:

  numeric value giving the desired standard deviation

## Value

a vector of the same length as `y` that has correlation `rho` with `y`.

## Examples

``` r
x <- runif(1000, 5, 8)

# create a random vector with positive correlation
y1 <- rand_with_cor(x, 0.8)
all.equal(cor(x, y1), 0.8)
#> [1] TRUE

# create a random vector with negative correlation
# and fixed mean and standard deviation
y2 <- rand_with_cor(x, -0.3, 2, 3)
all.equal(cor(x, y2), -0.3)
#> [1] TRUE
all.equal(mean(y2), 2)
#> [1] TRUE
all.equal(sd(y2), 3)
#> [1] TRUE
```
