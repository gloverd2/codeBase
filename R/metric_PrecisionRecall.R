#' metric_PrecisionRecall
#'
#' @description
#' Return Weighted mean of precisions achieved at each threshold
#'
#' @param actual Array[Numeric] - 0 or 1 - Values we are aiming to predict.
#' @param predicted Array[Numeric] / DataFrame[Numeric] - Between 0 and 1 - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used.
#' @param na.rm logical. Should missing values be removed?
#'
#' @seealso \code{\link{plot_PrecisionRecall}}
#'
#' @return Weighted mean of precisions achieved at each threshold, with the increase in recall from the previous threshold used as the weight. Single value if \code{predicted} is vector. Named list if \code{predicted} is dataframe.
#' @export
#'
#' @examples
#'
#' data <- data.frame(x1=runif(100), x2=runif(100), noise=rnorm(100, sd=0.2)) %>%
#'   mutate(target=ifelse(x1 + noise>0.5, 1, 0))
#'
#' metric_PrecisionRecall(actual=data$target, predicted=data$x1)
#' metric_PrecisionRecall(actual=data$target, predicted=data[c("x1","x2")])
metric_PrecisionRecall <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE){

  # Check weight
  checkmate::assert_numeric(weight, len=length(actual), lower=0)


  # Deal with predicted as dataframe or numeric vector
  if (is.numeric(predicted)){
    checkmate::assert_numeric(predicted, len=length(actual))

    # Calculate area under the predicted curve
    pr_df <- null_PrecisionRecall(actual=actual, predicted=predicted, weight=weight, na.rm=na.rm)
    # trapezium rule - sum up. Average height ((y1+y2)/2) between two points * width between points (x2-x1)
    #pr_auc_score <- sum(((dplyr::lag(pr_df["Precision"]) + pr_df["Precision"]) * (dplyr::lag(pr_df["Recall"]) - pr_df["Recall"]))/2, na.rm=TRUE)

    pr_ave_score <- sum((pr_df["Recall"]-dplyr::lead(pr_df["Recall"])) * pr_df["Precision"], na.rm =TRUE)

    if (any(is.na(pr_df["Precision"]), is.na(pr_df["Recall"]))){
      return(NA)
    }else{
      return(pr_ave_score)
    }

  }else{
    checkmate::assert_data_frame(predicted, nrows=length(actual))

    out_list <- list()
    for (ii in 1:ncol(predicted)){
      out_list[colnames(predicted)[ii]] <- metric_PrecisionRecall(actual, predicted[[ii]], weight=weight)
    }
    return(out_list)
  }

}

