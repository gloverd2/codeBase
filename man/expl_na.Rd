% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expl_na.R
\name{expl_na}
\alias{expl_na}
\title{expl_na}
\usage{
expl_na(df, na.strings = NULL, ignore.case = FALSE)
}
\arguments{
\item{df}{dataframe}

\item{na.strings}{Character - values to treat as NA}

\item{ignore.case}{Logical - TRUE/FALSE as to whether na.strings should be case specific}
}
\value{
a dataframe containing the columns:
var [Character]: variables from dataframe
na [Integer]: count of missing values
perc [Numeric]: percentage of that level

no columns: "no columns provided"
}
\description{
When given a dataframe object, this function will return
a dataframe object with 1 row per variable with the sum of NA's.
sorted alphabetically.
}
\note{
an example for na.strings is c("NA", "Missing", "no_xml")
}
\examples{
df <- data.frame(pet = rep(c('dog', 'cat', 'Missing', 'Unknown'), c(40,30,20,10)),
                age = rep(c(1:9,NA),10),
                sex = rep(c('M','F', NA), c(50,30,20)),
                stringsAsFactors = FALSE)
expl_na(df)
expl_na(df, na.strings = c('Missing'))
expl_na(df, na.strings = c('unknown'), ignore.case = TRUE)

}
\seealso{
expl_categorical
}
