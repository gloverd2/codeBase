#' metric_pode
#' @description
#' Returns the proportion of deviance explained by the predictions.
#' Note: Predictions should be annualized (independent of exposure)
#' Note: high is good. 1 is perfect model, 0 is null model, negative is worse than null model
#'
#' @section Inputs:
#' @template param-metric
#' @template param-metric_family
#'
#' @return Numeric: value of proportion of deviance
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
#' metric_pode(actual, predicted)
#' metric_pode(actual, predicted, weight)
#' metric_pode(actual, predicted, weight, family="gamma")
#'
#' @export
metric_pode <- function(actual, predicted, weight=rep(1, length(actual)), family="gaussian", na.rm=FALSE, rebase=FALSE, tweedie_power=NULL){

  # Error catching #Not needed as called in metric_deviance
  #metric_error_checking_family(actual, predicted, weight, family, na.rm, rebase, tweedie_power)

  # Rebase if required
  if (rebase){
    shift <- (mean(actual * weight, na.rm=na.rm)/mean(weight[!is.na(actual)], na.rm=na.rm)) - (mean(predicted * weight, na.rm=na.rm) / mean(weight[!is.na(predicted)], na.rm=na.rm))
    predicted <- predicted + shift
  }

  null_predicted <-  rep(sum(actual * weight, na.rm = na.rm) / sum(weight[!is.na(actual)], na.rm = na.rm), length(actual))

  # Might need changing when we sort out these functions metric_nloglik
  null_deviance = metric_deviance(actual, null_predicted, weight, family, na.rm, rebase, tweedie_power)
  pred_deviance = metric_deviance(actual, predicted,      weight, family, na.rm, rebase, tweedie_power)

  #Deal with NAs in input
  if (na.rm==FALSE & any(is.na(c(actual, predicted)))){
    return(NA)
  }

  if (null_deviance==0){
    warning("In metric_pode the null deviance is 0 (the actuals are all the same)")
    return(NA)
    }
  else{
    pode = 1- (pred_deviance/null_deviance)
    return(pode)
  }
}
