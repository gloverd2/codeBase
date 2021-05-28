#' metric_error_checking_nofamily
#' @description
#' function containing all of the error catching for calculating model metrics
#' This includes vectors being of the correct length and type
#'
#' @section Inputs:
#' @template param-metric
metric_error_checking_nofamily <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE, rebase=FALSE){

  checkmate::assert_true(length(actual)==length(predicted))
  checkmate::assert_numeric(actual, finite = TRUE)
  checkmate::assert_numeric(predicted, finite = TRUE)

  #Check weight
  checkmate::assert_numeric(weight, lower=0, finite = TRUE)
  checkmate::assert_true(length(actual)==length(weight))
  checkmate::assert_true(sum(weight, na.rm=TRUE) > 0)

  checkmate::assert_logical(na.rm, len=1)
  checkmate::assert_logical(rebase, len=1)

}


#' metric_error_checking_family
#' @description
#' function containing all of the error catching for calculating model metrics
#' This includes vectors being of the correct length and type, actuals and predictions agreeing with the distribution family
#' and the distribution family being allowed
#'
#' @section Inputs:
#' @template param-metric
#' @template param-metric_family
metric_error_checking_family <- function(actual, predicted, weight=rep(1, length(actual)), family="gaussian", na.rm=FALSE, rebase=FALSE, tweedie_power=NULL){

  checkmate::assert_true(length(actual)==length(predicted))
  #Check weight
  checkmate::assert_numeric(weight, lower=0, finite = TRUE)
  checkmate::assert_true(length(actual)==length(weight))
  checkmate::assert_true(sum(weight, na.rm=TRUE) > 0)

  checkmate::assert_choice(toupper(family), toupper(c("gaussian", "poisson", "gamma", "tweedie", "binomial")))


  # Check inputs agree with the families
  if(toupper(family)==toupper("gaussian")){
    checkmate::assert_choice(tweedie_power, choices=c(0), null.ok=TRUE) #tweedie_power is 0 for gaussian
    checkmate::assert_numeric(actual, finite = TRUE)
    checkmate::assert_numeric(predicted, finite = TRUE)
  }else if (toupper(family)==toupper("poisson")){
    checkmate::assert_choice(tweedie_power, choices=c(1), null.ok=TRUE) #tweedie_power is 1 for poisson
    checkmate::assert_numeric(actual, lower = 0)
    checkmate::expect_numeric(predicted, lower = 0)

  }else if (toupper(family) == toupper("gamma")){
    checkmate::assert_choice(tweedie_power, choices=c(2), null.ok=TRUE) #tweedie_power is 2 for gamma
    checkmate::assert_numeric(actual, lower = 0)
    checkmate::expect_numeric(predicted, lower = 0)

  }else if (toupper(family) == toupper("tweedie")){
    checkmate::assert_numeric(actual, lower = 0)
    checkmate::expect_numeric(predicted, lower = 0)
    checkmate::expect_numeric(tweedie_power, lower=0, upper=3)

  }else if (toupper(family) == toupper("binomial")){
    checkmate::assert_integerish(actual, lower = 0, upper = 1)
    checkmate::expect_numeric(predicted, lower = 0, upper = 1)

    if (length(unique(actual))<2){warning("binomial actual only has single target present")}
  }


  checkmate::assert_logical(na.rm, len = 1)
  checkmate::assert_logical(rebase, len = 1)

}



#' metric_error_checking_classification
#' @description
#' function containing all of the error catching for calculating model metrics of classifications
#' This includes vectors being of the correct length and type, actuals and predictions agreeing with the distribution family
#'
#' @section Inputs:
#' @template param-metric
#' @template param-metric_family
metric_error_checking_classification <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE, threshold=0.5){

  checkmate::assert_true(length(actual)==length(predicted))
  checkmate::assert_integerish(actual, lower = 0, upper = 1)
  if (length(unique(actual))<2){warning("binomial actual only has single target present")}

  checkmate::assert_numeric(predicted, lower = 0, upper = 1)

  #Check weight
  checkmate::assert_numeric(weight, lower=0, finite = TRUE)
  checkmate::assert_true(length(actual)==length(weight))
  checkmate::assert_true(sum(weight, na.rm=TRUE) > 0)


  checkmate::assert_logical(na.rm, len = 1)
  checkmate::assert_numeric(threshold, lower = 0, upper = 1, len = 1)

}

