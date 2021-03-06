% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_PrecisionRecall.R
\name{metric_PrecisionRecall}
\alias{metric_PrecisionRecall}
\title{metric_PrecisionRecall}
\usage{
metric_PrecisionRecall(
  actual,
  predicted,
  weight = rep(1, length(actual)),
  na.rm = FALSE
)
}
\arguments{
\item{actual}{Array[Numeric] - 0 or 1 - Values we are aiming to predict.}

\item{predicted}{Array[Numeric] / DataFrame[Numeric] - Between 0 and 1 - Values that we have predicted.}

\item{weight}{Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used.}

\item{na.rm}{logical. Should missing values be removed?}
}
\value{
Weighted mean of precisions achieved at each threshold, with the increase in recall from the previous threshold used as the weight. Single value if \code{predicted} is vector. Named list if \code{predicted} is dataframe.
}
\description{
Return Weighted mean of precisions achieved at each threshold
}
\examples{

data <- data.frame(x1=runif(100), x2=runif(100), noise=rnorm(100, sd=0.2)) \%>\%
  mutate(target=ifelse(x1 + noise>0.5, 1, 0))

metric_PrecisionRecall(actual=data$target, predicted=data$x1)
metric_PrecisionRecall(actual=data$target, predicted=data[c("x1","x2")])
}
\seealso{
\code{\link{plot_PrecisionRecall}}
}
