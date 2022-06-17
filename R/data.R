#' Dataset mtcars without row names
#'
#' In the [`mtcars`] dataset, the names of the car models are
#' stored as row names. However, when working with `ggplot2` and other
#' packages from the `tidyverse`, it is convenient to have all data in columns.
#' `mtcars2` is a variant of `mtcars` that contains car models in a column
#' instead of storing them as row names.
#' `mtcars_na` is the same dataset as `mtcars2`, but some of the columns
#' contain missing values.
#'
#' @format A data frame with 32 rows and 12 variables. The format is identical
#'  to [`mtcars`] and details can be found in its documentation. The only
#'  difference is that the car model names are stored in the column `model`
#'  instead of the row names.
#'
#' @keywords datasets

"mtcars2"

#' @rdname mtcars2
#' @format NULL

"mtcars2_na"


#' Summarised Data on Restaurant Bills
#'
#' Summary of data on restaurant bills from the dataset [`reshape2::tips`].
#' Labels are in German.
#'
#' @format A data frame with 8 rows and 4 variables:
#' \describe{
#'   \item{sex}{sex of the bill payer}
#'   \item{time}{time of day}
#'   \item{smoker}{whether there were smokers in the party}
#'   \item{mean_bill}{mean of all the bills in dollars}
#' }
#'
#' @keywords datasets

"bills"


#' Road Casualties in Great Britain 1969-84
#'
#' Extract of the data in the [`Seatbelts`] dataset as a data frame. The
#' original dataset is a multiple time series (class `mts`). Labels are
#' in German.
#'
#' @format A data frame with 576 rows and 3 variables:
#' \describe{
#'   \item{date}{data of the first data of the month for which the
#'     data was collected.}
#'   \item{seat}{seat where the persons that were killed or seriously
#'     injured were seated. One of "Fahrer" (driver's seat), "Beifahrer"
#'     (front seat), "Rücksitz" (rear seat).}
#'   \item{victims}{number of persons that were killed or seriously injured.}
#' }
#'
#' @keywords datasets

"seatbelts"

#' Galton's data on the heights of fathers and their children
#'
#' Two tables of father's heights with heights of one of their sons
#' (`galton_sons`) or daughters (`galton_daughters`), respectively. All heights
#' are given in centimetres. It is created from [`HistData::GaltonFamilies`] by
#' randomly selecting one son or daughter per family. Since some families consist
#' of only sons or only daughters, not all families are contained in both tables.
#'
#' @format Two data frames with 179 (`galton_sons`) or 176 (`galton_daughters`),
#'  respectively, and 2 variables:
#' \describe{
#'   \item{father}{size of the father in cm.}
#'   \item{son/daughter}{size of the son or daughter, respectively, in cm.}
#' }

#' @keywords datasets

"galton_sons"

#' @rdname galton_sons
#' @format NULL

"galton_daughters"


#' Dentition of Mammals
#'
#' [Dental formulas](https://en.wikipedia.org/wiki/Dentition#Dental_formula)
#' for various mammals. The dental formula describes the number of incisors,
#' canines, premolars and molars per quadrant. Upper and lower teeth may
#' differ and are therefore shown separately. The total number of teeth
#' is twice the number given.
#'
#' @format Data frame with 66 rows and 9 variables:
#' \describe{
#'   \item{name}{name of the mammal}
#'   \item{I}{number of top incisors}
#'   \item{i}{number of bottom incisors}
#'   \item{C}{number of top canines}
#'   \item{c}{number of bottom canines}
#'   \item{P}{number of top premolars}
#'   \item{p}{number of bottom premolars}
#'   \item{M}{number of top molars}
#'   \item{m}{number of bottom molars}
#' }
#'
#' @source
#' The data have been downloaded from
#' <https://people.sc.fsu.edu/~jburkardt/datasets/hartigan/file19.txt>
#'
#' They come from the following textbook:
#'
#' Hartigan, J. A. (1975). *Clustering Algorithms*, John Wiley, New York.
#'
#' Table 9.1, page 170.
#'
#' @keywords datasets

"dentition"


#' Protein Consumption in European Countries
#'
#' Protein Consumption from various sources in European countries in
#' unspecified units. The exact year of data collection is not known but the
#' oldest known publication of the data is from 1973.
#'
#' @format Data frame with 25 rows and 10 variables:
#' \describe{
#'   \item{country}{name of the country}
#'   \item{red_meat}{red meat}
#'   \item{white_meat}{white meat}
#'   \item{eggs}{eggs}
#'   \item{milk}{milk}
#'   \item{fish}{fish}
#'   \item{cereals}{cereals}
#'   \item{starch}{starchy foods}
#'   \item{nuts}{pulses, nuts, oil-seeds}
#'   \item{fruit_veg}{fruits, vegetables}
#' }
#'
#' @source
#' The data have been downloaded from
#' <https://raw.githubusercontent.com/jgscott/STA380/master/data/protein.csv>
#'
#' They come from the following book:
#'
#' Hand, D. J. et al. (1994). *A Handbook of Small Data Sets*,
#' Chapman and Hall, London.
#'
#' Chapter 360, p. 297.
#'
#' In the book, it is stated that the data have first been published in
#'
#' Weber, A. (1973).
#' *Agrarpolitik im Spannungsfeld der internationalen Ernährungspolitik*,
#' Institut für Agrarpolitik und Marktlehre, Kiel.
#'
#' @keywords datasets

"protein"


#' Wine Quality
#'
#' Physicochemical data and quality ratings for red and white Portuguese
#' [Vinho Verde](https://en.wikipedia.org/wiki/Vinho_Verde) wines.
#'
#' @format a tibble with 6497 rows and 13 variables:
#' \describe{
#'   \item{colour}{colour of the wine; "red" (1'599) or "white" (4'898)}
#'   \item{fixed_acidity}{tartaric acid per volume in \eqn{g/dm^3}}
#'   \item{volatile_acidity}{acetic acid per volume in \eqn{g/dm^3}}
#'   \item{citric_acid}{citric acid per volume in \eqn{g/dm^3}}
#'   \item{residual_sugar}{residual sugar per volume in \eqn{g/dm^3}}
#'   \item{chlorides}{sodium chloride per volume in \eqn{g/dm^3}}
#'   \item{free_sulfur_dioxide}{free sulphur dioxide  per volume in \eqn{mg/dm^3}}
#'   \item{total_sulfur_dioxide}{total sulphur dioxide per volume in \eqn{mg/dm^3}}
#'   \item{density}{density in \eqn{g/dm^3}}
#'   \item{pH}{pH value}
#'   \item{sulphates}{potassium sulphate per volume in \eqn{g/dm^3}}
#'   \item{alcohol}{alcohol content per volume in %}
#'   \item{quality}{quality score between 0 (worst) and 10 (best) determined
#'    by sensory analysis.}
#' }
#'
#' @source
#' The data is available on the
#' [UC Irvine Machine Learning Repository](https://archive-beta.ics.uci.edu/ml/datasets/wine+quality).
#'
#' P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis,
#' *Modeling wine preferences by data mining from physicochemical properties*,
#' Decision Support Systems 47(4) (2009), 547-553.
#'
#' @keywords datasets

"wine_quality"


#' Wisconsin Breast Cancer Database
#'
#' Breast cancer database obtained from the University of Wisconsin
#' Hospitals, Madison from Dr. William H. Wolberg. The data were collected
#' in 8 from 1989 to 1991 and are sorted in chronological order.
#'
#' @format a tibble with 699 rows and 11 variables. All numerical values are
#'  integers in the range 1 to 10.
#' \describe{
#'   \item{id}{sample code number}
#'   \item{clump_thick}{clump thickness}
#'   \item{unif_cell_size}{uniformity of cell size}
#'   \item{unif_cell_shape}{uniformity of cell shape}
#'   \item{marg_adh}{marginal adhesion}
#'   \item{ep_cell_size}{single epithelial cell size}
#'   \item{bare_nucl}{bare nuclei}
#'   \item{bland_chromat}{bland chromatin}
#'   \item{norm_nucl}{normal nucleoli}
#'   \item{mitoses}{mitoses}
#'   \item{class}{"benign" (458) or "malignant" (241)}
#' }
#'
#' @source
#' The data is available on the
#' [UC Irvine Machine Learning Repository](https://archive-beta.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+original).
#'
#' O. L. Mangasarian and W. H. Wolberg, *Cancer diagnosis via linear programming*,
#' SIAM News, Volume 23(5) (1990) 1 & 18.
#'
#' @keywords datasets

"breast_cancer"


#' Noisy Data From a Tenth Order Polygon
#'
#' Training and test data create from a tenth order polynomial with added noise.
#' The polynomial is given by
#' \deqn{f(x) = 2 x - 10 x^5 + 15 x^{10}}{f(x) = 2 x - 10 x^5 + 15 x^10}
#' The noise follows a standard normal distribution. The data can be used to
#' demonstrate overfitting. It is inspired by section II. B. in
#' [A high-bias, low-variance introduction to Machine Learning for physicists](https://arxiv.org/abs/1803.08823)
#'
#' @format a list of two tibbles with two columns each. \eqn{x} stands for the
#'  independent, \eqn{y} for the dependent variable. The training data
#'  (`noisy_data$train`) contains 1000 rows, the test data (`noisy_data$test`)
#'  20 rows.
#'
#' @references
#' P. Mehta et al.,
#' *A high-bias, low-variance introduction to Machine Learning for physicists*
#' Phys. Rep. 810 (2019), 1-124.
#' [arXiv:1803.08823](https://arxiv.org/abs/1803.08823)
#' \doi{10.1016/j.physrep.2019.03.001}
#'
#' @keywords datasets

"noisy_data"


#' History of the Number of Available CRAN Packages
#'
#' Table with the number of packages available on CRAN and the current R version
#' for historic dates back to 21 June 2001.
#'
#' @format Data frame with 25 rows and 10 variables. The first column (`Country`)
#'  indicates the name of the country, the other columns indicate protein
#'  consumption from nine sources sources in unknown units.
#'
#' @details
#' Data on the number of packages on CRAN between 2001-06-21 and 2014-04-13
#' is obtained from
#' [`CRANpackages`](https://www.rdocumentation.org/packages/Ecdat/versions/0.3-9/topics/CRANpackages)
#' from the package [`Ecdat`](https://cran.r-project.org/package=Ecdat).
#' This data was collected by John Fox and Spencer Graves.
#' Intervals between data points are irregularly spaced. These data are
#' marked with `John Fox` or `Spencer Graves` in the column `source`.
#' They are licenced under GPL-2/GPL-3.
#'
#' Newer data was obtained using the functions [`n_available_packages()`] and
#' [`available_r_version()`] which extract the information from CRAN snapshots
#' on MRAN. One data point per quarter is available starting on 2014-10-01.
#' These data are marked with `MRAN` in the column `source`.
#'
#' @examples
#' library(ggplot2)
#' ggplot(cran_history, aes(x = date, y = n_packages)) +
#'   geom_point()
#'
#' @keywords datasets

"cran_history"

#' Get Files for File Reading Exercise
#'
#' Copy the files for an exercise for reading files to a directory.
#'
#' @param path path where the files should be copied to.
#' @param unzip logical indicating whether the files should be unzipped. Set this
#'  to `FALSE` if unzipping fails.
#'
#' @details
#' There are 8 files in total.
#' Apart from a few errors that were introduced for the purpose of the exercise,
#' they all contain the same data: information about 100 randomly selected
#' Swiss municipalities. The full file can be downloaded from
#' <https://www.bfs.admin.ch/bfsstatic/dam/assets/7786544/master>.
#'
#' @return
#' Logical indicating the success of the copy operation.
#'
#' @export

get_reading_exercise_files <- function(path, unzip = TRUE) {

  if (!dir.exists(path)) {
    stop("Directory ", path, " does not exist.")
  }

  zipfile <- system.file("extdata", "files.zip", package = "ibawds")

  if (unzip) {
    utils::unzip(zipfile, exdir = path)
    success <- TRUE
  } else {
    success <- file.copy(zipfile, path)
  }

  invisible(success)
}
