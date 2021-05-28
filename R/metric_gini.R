#' metric_gini
#'
#' @description
#' Returns gini coeffient of one or more models
#'
#' Note: Predictions should be annualised (independent of exposure)
#'
#' @param actual Array[Numeric] - Values we are aiming to predict.
#' @param predicted Array[Numeric] / DataFrame[Numeric] - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used.
#' @param na.rm logical. Should missing values be removed?
#'
#' @seealso \code{\link{plot_gini}}
#'
#' @return gini coeffient. Single value if \code{predicted} is vector. Named list if \code{predicted} is dataframe.
#' @export
#'
#' @examples
#'
#' # Input as vector
#' actual <- rnorm(100, mean=100, sd=10)
#' weight <- rep(1,100)
#' predicted <- actual + rnorm(100, mean=0, sd=1)
#'
#' metric_gini(actual, predicted, weight)
#'
#' # Input as dataframe
#'
#' predicted <- data.frame(pred1 = actual + rnorm(100, mean=0, sd=1), pred2 = rnorm(100, mean=0, sd=1))
#' metric_gini(actual, predicted, weight)
#'
metric_gini <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE){

  # Data prep and checking --------------------------------------------------
  checkmate::assert_numeric(actual, lower=0)
  checkmate::assert_logical(na.rm, len=1)
  checkmate::assert_numeric(weight, len=length(actual), lower=0)



  # Ideal GINI --------------------------------------------------------------


  data_t_act <- null_gini(actual=actual, predicted=actual, weight=weight, na.rm=na.rm)

  # Calculate ideal AUC coefficent
  # trapezium rule for area under a curve then subtract 0.5 for area above y=x line
  # trapezium rule - sum up. Average height ((y1+y2)/2) between two points * width between points (x2-x1)
  AUC_ideal <- sum(((dplyr::lag(data_t_act[["cum_act"]]) + data_t_act[["cum_act"]]) * (data_t_act[["cum_pop"]] - dplyr::lag(data_t_act[["cum_pop"]])))/2, na.rm=TRUE) - 0.5

  # Prediction GINI --------------------------------------------------------------
  # Deal with predicted as dataframe or numeric vector
  if (is.numeric(predicted)){
    checkmate::assert_numeric(predicted, len=length(actual))

    data_t_pred <- null_gini(actual=actual, predicted=predicted, weight=weight, na.rm=na.rm)

    # Calculate area under curve and normalised gini
    # trapezium rule for area under a curve then subtract 0.5 for area above y=x line
    # trapezium rule - sum up. Average height ((y1+y2)/2) between two points * width between points (x2-x1)
    AUC_pred <- sum(((dplyr::lag(data_t_pred[["cum_act"]]) + data_t_pred[["cum_act"]]) * (data_t_pred[["cum_pop"]] - dplyr::lag(data_t_pred[["cum_pop"]])))/2, na.rm=TRUE) - 0.5
    # normalised to caluculate gini
    gini <- AUC_pred/AUC_ideal

    if (any(is.na(data_t_pred["cum_act"]), is.na(data_t_pred["cum_act"]))){
      return(NA)
    }else{
      return(gini)
    }

  }else{
    checkmate::assert_data_frame(predicted, nrows=length(actual))

    out_list <- list()
    for (ii in 1:ncol(predicted)){
      out_list[colnames(predicted)[ii]] <- metric_gini(actual=actual, predicted=predicted[[ii]], weight=weight)
    }
    return(out_list)
  }
}
