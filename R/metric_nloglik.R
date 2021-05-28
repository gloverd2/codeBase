#' metric_nloglik
#' @description
#' Returns the negative log likelihood for the predictions.
#' Note: Predictions should be annualized (independent of exposure)
#' Note: Large negative values are good. (function should be minimised)
#'
#' @section Inputs:
#' @template param-metric
#' @template param-metric_family
#'
#' @return Numeric: negative log likelihood
#'
#' @family Metrics
#'
#' @examples
#'
#' set.seed(666)
#' actual <- pmax(rep(10, 10), 0)
#' predicted <- pmax(rnorm(n = 10, mean = 10, sd = 1), 0)
#' weight <- pmax(rnorm(n = 10, mean = 10, sd = 1), 0)
#'
#' metric_nloglik(actual, predicted, family="poisson")
#' metric_nloglik(actual, predicted, weight, family="poisson")
#' metric_nloglik(actual, predicted, weight, family="gamma")
#' metric_nloglik(actual=c(rep(0,5), rep(1,5)), predicted=seq(0.01,0.99,length.out=10), weight, family="binomial")
#'
#' @export
metric_nloglik <- function(actual, predicted, weight=rep(1, length(actual)), family="gaussian", na.rm=FALSE, rebase=FALSE, tweedie_power=NULL){

  # Error catching
  metric_error_checking_family(actual, predicted, weight, family, na.rm, rebase, tweedie_power)

  # Rebase if required
  if (rebase){
    shift <- (mean(actual * weight, na.rm=na.rm)/mean(weight[!is.na(actual)], na.rm=na.rm)) - (mean(predicted * weight, na.rm=na.rm) / mean(weight[!is.na(predicted)], na.rm=na.rm))
    predicted <- predicted + shift
  }

  # logic copied from
  # https://documentation.sas.com/?docsetId=statug&docsetTarget=statug_genmod_details01.htm&docsetVersion=14.3&locale=en#statug.genmod.genmodll

  if (toupper(family)==toupper("gaussian")){

    dispersion <- var(actual, na.rm=na.rm)
    part1 <- (weight * ((actual - predicted)^2))/dispersion
    part2 <- (log(dispersion/weight))

    logloss_ii <- -0.5 * (part1 + part2 + log(2*pi))
    logloss <- sum(logloss_ii, na.rm=na.rm) / sum(weight[!is.na(logloss_ii)], na.rm = na.rm)
    return(-logloss)

  }else if (toupper(family)==toupper("poisson")){

    predicted <- pmax(predicted, 1e-15) # remove 0s
    dispersion <- 1
    part1 <- actual * log(predicted)
    part2 <- predicted
    #part3 <- 0
    part3 <- log(factorial(actual))

    logloss_ii <- (weight / dispersion) * (part1 - part2 - part3)
    logloss <- sum(logloss_ii, na.rm=na.rm) / sum(weight[!is.na(logloss_ii)], na.rm = na.rm)
    return(-logloss)

  }else if(toupper(family)==toupper("gamma")){

    predicted <- pmax(predicted, 1e-15) # remove 0s
    dispersion <- var(actual) / mean(actual)^2
    part1 <- (weight / dispersion) * log((weight * actual) / (dispersion * predicted))
    part2 <- (weight * actual) / (dispersion * predicted)
    part3 <- log(actual)
    part4 <- log(gamma(weight / dispersion))

    logloss_ii <- part1 - part2 - part3 - part4
    logloss <- sum(logloss_ii, na.rm=na.rm) / sum(weight[!is.na(logloss_ii)], na.rm = na.rm)
    return(-logloss)
  }else if (toupper(family)==toupper("tweedie")){
    # https://towardsdatascience.com/tweedie-loss-function-for-right-skewed-data-2c5ca470678f
    # https://documentation.sas.com/?cdcId=pgmsascdc&cdcVersion=9.4_3.4&docsetId=statug&docsetTarget=statug_hpgenselect_details16.htm&locale=en#statug_hpgenselect003957

    if (tweedie_power==0){
      return(metric_nloglik(actual=actual, predicted=predicted, weight=weight, family="gaussian", na.rm=na.rm, rebase=rebase, tweedie_power=tweedie_power))
    }else if (tweedie_power==1){
      return(metric_nloglik(actual=actual, predicted=predicted, weight=weight, family="poisson", na.rm=na.rm, rebase=rebase, tweedie_power=tweedie_power))
    }else if (tweedie_power==2){
      return(metric_nloglik(actual=actual, predicted=predicted, weight=weight, family="gamma", na.rm=na.rm, rebase=rebase, tweedie_power=tweedie_power))
    }else{

      part1 <- actual * (predicted^(1-tweedie_power))/(1-tweedie_power)
      part2 <- (predicted^(2-tweedie_power))/(2-tweedie_power)

      logloss_ii <- (part1 - part2)
      logloss <- sum(logloss_ii * weight, na.rm=na.rm) / sum(weight[!is.na(logloss_ii)], na.rm = na.rm)
      return(-logloss)
    }


  }else if (toupper(family)==toupper("binomial")){

    if (max(predicted)==1 | min(predicted)==0){
      warning("metric_nloglik takes probability predictions not binary predictions")
      predicted <- pmin(pmax(predicted, 1e-15), 1-1e-15) # remove 0s and 1s
    }
    part1 <- actual * log(predicted)
    part2 <- (1 - actual) * log(1 - predicted)

    logloss_ii <- weight * (part1 + part2)
    logloss <- sum(logloss_ii, na.rm=na.rm) / sum(weight[logloss_ii], na.rm = na.rm)
    return(-logloss)
  }else{

    warning('metric_nloglik only implemented for gaussian, poisson, gamma, binomial')
    return(NA)
  }
}
