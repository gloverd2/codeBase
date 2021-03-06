% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_all_identical.R
\name{check_all_identical}
\alias{check_all_identical}
\title{check_all_identical}
\usage{
check_all_identical(input)
}
\arguments{
\item{input}{List - This is the list of values to perform an identical check on}
}
\value{
Boolean - TRUE if all are identical. FALSE if they are not all identical.
}
\description{
This is a basic function which takes a list and checks that every value within the list is identical.
It is simply a recursive function as any failure would result in a FALSE.
Note: Identicality is a stronger condition than Equality. E.g 1L == 1 but identicaL(1L,1) is FALSE.
}
\section{Functions dependent on this}{
 prep_char_num_sort
}

\examples{
check_all_identical(as.list(c(NA,NA,NA)))
check_all_identical(list(1,1L,1.00001))
check_all_identical(as.list(c("A","A","A","B")))
check_all_identical(list(as.double(NA),as.logical(NA)))
check_all_identical(list(1,1,1,1,1L))
}
