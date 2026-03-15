# Simulated Dice Throws

A list with 6 numeric vectors containing the result of a number of
simulated throws with a six-sided dice. Not all of the dice are fair and
they are unfair in different ways.

## Usage

``` r
dice_data
```

## Format

a list containing 6 numeric vectors with varying length between 158 and
1027. The elements of the list are named "d1", "d2", etc.

## Examples

``` r
# the numeric vectors differ in length
lengths(dice_data)
#>   d1   d2   d3   d4   d5   d6 
#>  174  158  207  168 1027  784 

# compute the mean for each dice
sapply(dice_data, mean)
#>       d1       d2       d3       d4       d5       d6 
#> 3.867816 3.670886 3.347826 3.797619 3.591042 3.709184 

# look at the contingency table for dice 3
table(dice_data$d3)
#> 
#>  1  2  3  4  5  6 
#> 34 47 34 31 27 34 
```
