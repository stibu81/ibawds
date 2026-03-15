# Check If the User Is Ready for the Course

Check if the current system is ready for the course by verifying the
following:

- R and RStudio are up to date

- the ibawds package is up to date

- all the required packages are installed

The function must be run from RStudio in order to run properly.

## Usage

``` r
check_ibawds_setup()
```

## Value

a logical indicating whether the system is up to date (invisibly).
Messages inform the user about the status of the system.
