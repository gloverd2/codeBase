#' metric_mae
#' @description
#' Returns the mean absolute error for a set of predictions.
#' Note: Predictions should be annualized (independent of exposure)
#' Note: Low is good
#'
#' @section Inputs:
#' @template param-metric
#' @param family can be NULL has this metric doesn't depend on family
#'
#' @return Numeric: value of mean absolute error
#'
#' @family Metrics
#'
#' @examples
#'
#' set.seed(666)
#' actual <- rep(10, 10)
#' predicted <- rnorm(n = 10, mean = 10, sd = 1)
#' weight <- pmax(rnorm(n = 10, mean = 10, sd = 1) , 0)
#'
#' metric_mae(actual, predicted)
#' metric_mae(actual, predicted, weight)
#'
#' @export
metric_mae <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE, rebase=FALSE){

  # Error catching
  metric_error_checking_nofamily(actual, predicted, weight, na.rm, rebase)

  # Rebase if required
  if (rebase){
    shift <- (mean(actual * weight, na.rm=na.rm)/mean(weight[!is.na(actual)], na.rm=na.rm)) - (mean(predicted * weight, na.rm=na.rm) / mean(weight[!is.na(predicted)], na.rm=na.rm))
    predicted <- predicted + shift
  }

  mae = sum(abs(actual - predicted) * weight, na.rm=na.rm) / sum(weight[!is.na((actual - predicted))], na.rm=na.rm)

  return(mae)
}
