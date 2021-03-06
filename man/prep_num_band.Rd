% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_num_band.R
\name{prep_num_band}
\alias{prep_num_band}
\title{prep_num_band}
\usage{
prep_num_band(var_to_band, bands, out_of_bands = 99999999, output = "values")
}
\arguments{
\item{var_to_band}{Array[Numeric] - Values that we are trying to apply banding to.}

\item{bands}{Array[Numeric] - The upper limits of the bands we would like to apply.
Note: this must be sorted in ascending order.}

\item{out_of_bands}{Numeric (Default = 99,999,999) - This is a value which will get assigned to values above the top band specified.
Note: If you exceed this, it will be replaced by the maximum value of var_to_band or bands (depending on which is larger).
This is intended to be used to have a consistent value across functions for the top band upper limit.}

\item{output}{Character (Default: "values") - This can accept either "values" or "indices".
Using indices will return the indices of our list of bins to show which value is called.
Note: out_of_bands values will be assigned to the last bin.}
}
\value{
Array[Numeric] - If output == "values": var_to_band but with each value capped into the bands provided.
  If output == "indices": Return a list of the indices of the bands which were called.
}
\description{
This can band numeric values with a purely numeric vector or a vector of mixed entries.
Bands are inclusive in the upper bound.
An extra band of 99,999,999 will be added if there are values above the largest band.
e.g. If I have a value of 500 and bands of 200, 500 and 800: 500 will be in the 500 band but 500.01 would be in 800.

Note: Banding is consistent with how Pricing view bands in rate tables.
}
\section{Inputs}{

}

\examples{
prep_num_band(var_to_band=c(1, 2, 3, 4, 5, 6, 7, 8), bands=c(3, 5, 7))
# Output: (3, 3, 3, 5, 5, 7, 7, 99999999)

prep_num_band(var_to_band=c(-100, 200, 0, 1.5, 3.14159, 10, 7, -200), bands=c(-150, -10, 4, 5))
# Output: (-10, 99999999, 4, 4, 4, 99999999, 99999999, -150)

prep_num_band(var_to_band=c(-100, 200, 0, 1.5, 3.14159, 10, 7, -200), bands=c(-150, -10, 4, 5), out_of_bands = 999)
# Output: (-10, 999, 4, 4, 4, 999, 999, -150)

}
