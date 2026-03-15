# Road Casualties in Great Britain 1969-84

Extract of the data in the
[`Seatbelts`](https://rdrr.io/r/datasets/UKDriverDeaths.html) dataset as
a data frame. The original dataset is a multiple time series (class
`mts`). Labels are in German.

## Usage

``` r
seatbelts
```

## Format

A data frame with 576 rows and 3 variables:

- date:

  data of the first data of the month for which the data was collected.

- seat:

  seat where the persons that were killed or seriously injured were
  seated. One of "Fahrer" (driver's seat), "Beifahrer" (front seat),
  "Rücksitz" (rear seat).

- victims:

  number of persons that were killed or seriously injured.
