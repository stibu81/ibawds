# Galton's data on the heights of fathers and their children

Two tables of father's heights with heights of one of their sons
(`galton_sons`) or daughters (`galton_daughters`), respectively. All
heights are given in centimetres. It is created from
`HistData::GaltonFamilies` by randomly selecting one son or daughter per
family. Since some families consist of only sons or only daughters, not
all families are contained in both tables.

## Usage

``` r
galton_sons

galton_daughters
```

## Format

Two data frames with 179 (`galton_sons`) or 176 (`galton_daughters`)\$
rows, respectively, and 2 variables:

- father:

  size of the father in cm.

- son/daughter:

  size of the son or daughter, respectively, in cm.
