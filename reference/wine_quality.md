# Wine Quality

Physicochemical data and quality ratings for red and white Portuguese
[Vinho Verde](https://en.wikipedia.org/wiki/Vinho_Verde) wines.

## Usage

``` r
wine_quality
```

## Format

a tibble with 6497 rows and 13 variables:

- colour:

  colour of the wine; "red" (1'599) or "white" (4'898)

- fixed_acidity:

  tartaric acid per volume in \\g/dm^3\\

- volatile_acidity:

  acetic acid per volume in \\g/dm^3\\

- citric_acid:

  citric acid per volume in \\g/dm^3\\

- residual_sugar:

  residual sugar per volume in \\g/dm^3\\

- chlorides:

  sodium chloride per volume in \\g/dm^3\\

- free_sulfur_dioxide:

  free sulphur dioxide per volume in \\mg/dm^3\\

- total_sulfur_dioxide:

  total sulphur dioxide per volume in \\mg/dm^3\\

- density:

  density in \\g/dm^3\\

- pH:

  pH value

- sulphates:

  potassium sulphate per volume in \\g/dm^3\\

- alcohol:

  alcohol content per volume in %

- quality:

  quality score between 0 (worst) and 10 (best) determined by sensory
  analysis.

## Source

The data is available on the [UC Irvine Machine Learning
Repository](https://archive.ics.uci.edu/dataset/186/wine+quality).

P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis, *Modeling wine
preferences by data mining from physicochemical properties*, Decision
Support Systems 47(4) (2009), 547-553.
