#' metric_precision
#' @description
#' Returns the precision TP / (TP + FP) of a classification using the confusion matrix
#' Note: Predictions should be annualized (independent of exposure)
#' Note: Perfect precision is 1, poor model is 0
#'
#'
#' @section Inputs:
#' @template param-metric_classification
#'
#' @return precision of classification TP / (TP + FP)
#' @export
#'
#' @examples
#'
#' metric_precision(actual=c(0,1,0,0), predicted=c(0.1,0.9,0.4,0.6))
#'
metric_precision <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE, threshold=0.5){

  # Error checking done in metric_confusion_matrix
  confusion_mat <- metric_confusion_matrix(actual, predicted, weight, na.rm, threshold)

  if (na.rm==FALSE & any(is.na(c(actual,predicted)))){
    return(NA)
  }

  return(confusion_mat["predicted.1", "actual.1"] / (confusion_mat["predicted.1", "actual.1"] + confusion_mat["predicted.1", "actual.0"]))

}
