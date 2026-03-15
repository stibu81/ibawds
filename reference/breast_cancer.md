# Wisconsin Breast Cancer Database

Breast cancer database obtained from the University of Wisconsin
Hospitals, Madison from Dr. William H. Wolberg. The data were collected
in 8 from 1989 to 1991 and are sorted in chronological order.

## Usage

``` r
breast_cancer
```

## Format

a tibble with 699 rows and 11 variables. All numerical values are
integers in the range 1 to 10.

- id:

  sample code number

- clump_thick:

  clump thickness

- unif_cell_size:

  uniformity of cell size

- unif_cell_shape:

  uniformity of cell shape

- marg_adh:

  marginal adhesion

- ep_cell_size:

  single epithelial cell size

- bare_nucl:

  bare nuclei

- bland_chromat:

  bland chromatin

- norm_nucl:

  normal nucleoli

- mitoses:

  mitoses

- class:

  "benign" (458) or "malignant" (241)

## Source

The data is available on the [UC Irvine Machine Learning
Repository](https://archive.ics.uci.edu/dataset/15/breast+cancer+wisconsin+originall).

O. L. Mangasarian and W. H. Wolberg, *Cancer diagnosis via linear
programming*, SIAM News, Volume 23(5) (1990) 1 & 18.
