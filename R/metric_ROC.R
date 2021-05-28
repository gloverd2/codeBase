#' metric_ROC
#'
#' @description
#' Return the area under the ROC curve
#'
#' @param actual Array[Numeric] - 0 or 1 - Values we are aiming to predict.
#' @param predicted Array[Numeric] / DataFrame[Numeric] - Between 0 and 1 - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used.
#' @param na.rm logical. Should missing values be removed?
#'
#' @seealso \code{\link{plot_ROC}}
#'
#' @return Area under the ROC curve. Single value if \code{predicted} is vector. Named list if \code{predicted} is dataframe.
#' @export
#'
#' @examples
#'
#' data <- data.frame(x1=runif(100), x2=runif(100), noise=rnorm(100, sd=0.2)) %>%
#'   mutate(target=ifelse(x1 + noise>0.5, 1, 0))
#'
#' metric_ROC(actual=data$target, predicted=data$x1)
#' metric_ROC(actual=data$target, predicted=data[c("x1","x2")])
metric_ROC <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE){

  # Check Weight
  checkmate::assert_numeric(weight, len=length(actual), lower=0)


  # Deal with predicted as dataframe or numeric vector
  if (is.numeric(predicted)){
    checkmate::assert_numeric(predicted, len=length(actual))

    roc_df <- null_ROC(actual=actual, predicted=predicted, weight=weight, na.rm=na.rm)

    # Calculate area under the curve
    # trapezium rule for area under a curve then subtract 0.5 for area above y=x line
    # trapezium rule - sum up. Average height ((y1+y2)/2) between two points * width between points (x2-x1)
    roc_auc_score <- sum(((dplyr::lag(roc_df["TP_Rate"]) + roc_df["TP_Rate"]) * (dplyr::lag(roc_df["FP_Rate"]) - roc_df["FP_Rate"]))/2, na.rm=TRUE)

    if (any(is.na(roc_df["TP_Rate"]), is.na(roc_df["FP_Rate"]))){
      return(NA)
    }else{
      return(roc_auc_score)
    }

  }else{
    checkmate::assert_data_frame(predicted, nrows=length(actual))

    out_list <- list()
    for (ii in 1:ncol(predicted)){
      out_list[colnames(predicted)[ii]] <- metric_ROC(actual, predicted[[ii]], weight=weight)
    }
    return(out_list)
  }

}

