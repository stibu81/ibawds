% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{dentition}
\alias{dentition}
\title{Dentition of Mammals}
\format{
Data frame with 66 rows and 9 variables:
\describe{
\item{name}{name of the mammal}
\item{I}{number of top incisors}
\item{i}{number of bottom incisors}
\item{C}{number of top canines}
\item{c}{number of bottom canines}
\item{P}{number of top premolars}
\item{p}{number of bottom premolars}
\item{M}{number of top molars}
\item{m}{number of bottom molars}
}
}
\source{
The data have been downloaded from
\url{https://people.sc.fsu.edu/~jburkardt/datasets/hartigan/file19.txt}

They come from the following textbook:

Hartigan, J. A. (1975). \emph{Clustering Algorithms}, John Wiley, New York.

Table 9.1, page 170.
}
\usage{
dentition
}
\description{
\href{https://en.wikipedia.org/wiki/Dentition#Dental_formula}{Dental formulas}
for various mammals. The dental formula describes the number of incisors,
canines, premolars and molars per quadrant. Upper and lower teeth may
differ and are therefore shown separately. The total number of teeth
is twice the number given.
}
\keyword{datasets}
