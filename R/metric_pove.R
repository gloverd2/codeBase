#' metric_pove
#' @description
#' Returns the proportion of variance explained by the predictions.
#' Note: Predictions should be annualized (independent of exposure)
#' Note: high is good. 1 is perfect model, 0 is null model, negative is worse than null model
#'
#' @section Inputs:
#' @template param-metric
#'
#' @return Numeric: value of proportion of variance explained
#'
#' @family Metrics
#'
#' @examples
#'
#' set.seed(666)
#' actual <- rnorm(n = 10, mean = 10, sd = 3)
#' predicted <- actual + rnorm(n = 10, mean = 0, sd = 1)
#' weight <- pmax(rnorm(n = 10, mean = 10, sd = 1) , 0)
#'
#' metric_pove(actual, predicted)
#' metric_pove(actual, predicted, weight)
#'
#' @export
metric_pove <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE, rebase=FALSE){

  # Error catching
  metric_error_checking_nofamily(actual, predicted, weight, na.rm, rebase)

  # Rebase if required
  if (rebase){
    shift <- (mean(actual * weight, na.rm=na.rm)/mean(weight[!is.na(actual)], na.rm=na.rm)) - (mean(predicted * weight, na.rm=na.rm) / mean(weight[!is.na(predicted)], na.rm=na.rm))
    predicted <- predicted + shift
  }

  #Deal with NAs in input
  if (na.rm==FALSE & any(is.na(c(actual, predicted)))){
    return(NA)
  }

  #Calculate the weighted variance
  weighted.var <- function(x, w, na.rm = FALSE){
    sum(w * (x - weighted.mean(x, w, na.rm = na.rm))^2, na.rm=na.rm) / sum(w, na.rm=na.rm)
  }

  actual.var <- weighted.var(actual, weight, na.rm=na.rm)
  remaining.var <- weighted.var(actual - predicted, weight, na.rm=na.rm)

  if (actual.var==0){return(NA)} # No variance to explain
  else{
    pove = 1- (remaining.var/actual.var)
  return(pove)
  }
}
