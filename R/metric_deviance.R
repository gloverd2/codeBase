#' metric_deviance
#' @description
#' Returns The deviance of the predictions.
#' Note: Predictions should be annualized (independent of exposure)
#' Note: Low is good. 0 is perfect model
#'
#' @section Inputs:
#' @template param-metric
#' @template param-metric_family
#'
#' @return Numeric: deviance of the predictions
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
#' metric_deviance(actual, predicted, family="gaussian")
#' metric_deviance(actual, predicted, weight, family="gaussian")
#'
#' @export
metric_deviance <- function(actual, predicted, weight=rep(1, length(actual)), family="gaussian", na.rm=FALSE, rebase=FALSE, tweedie_power=NULL){

  # Error catching
  metric_error_checking_family(actual, predicted, weight, family, na.rm, rebase, tweedie_power)

  # Rebase if required
  if (rebase){
    shift <- (mean(actual * weight, na.rm=na.rm)/mean(weight[!is.na(actual)], na.rm=na.rm)) - (mean(predicted * weight, na.rm=na.rm) / mean(weight[!is.na(predicted)], na.rm=na.rm))
    predicted <- predicted + shift
  }


  if (toupper(family) %in% toupper(c("gaussian", "poisson", "gamma", "tweedie"))){

    p <- ifelse(is.null(tweedie_power),-1,tweedie_power)

    # https://en.wikipedia.org/wiki/Tweedie_distribution#The_Tweedie_deviance
    if (p==0 | toupper(family)==toupper("gaussian")){
      deviance_vec <- (actual - predicted)^2
    }else if (p==1 | toupper(family)==toupper("poisson")){
      predicted <- pmax(predicted, 1e-15) # get rid of 0's from predictions
      deviance_vec <- 2 * ((actual * log(actual/predicted)) + predicted - actual)
    }else if (p==2| toupper(family)==toupper("gamma")){
      predicted <- pmax(predicted, 1e-15) # get rid of 0's from predictions
      deviance_vec <- 2 * (log(predicted/actual) + (actual/predicted) - 1)
    }else{
      predicted <- pmax(predicted, 1e-15) # get rid of 0's from predictions
      part1 <- (actual^(2-p)) / ((1-p)*(2-p))
      part2 <- actual * (predicted ^ (1-p)) / (1 - p)
      part3 <- (predicted ^ (2-p)) / (2 - p)
      deviance_vec <- 2 * (part1 - part2 + part3)
    }
    deviance <- sum(deviance_vec * weight, na.rm = na.rm) / sum(weight[!is.na(deviance_vec)], na.rm = na.rm)

  }else{warning("can't yet do binomial for deviance")}
  return(deviance)
}
