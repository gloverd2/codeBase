
#' metric_Fbeta
#' @description
#' Returns the Fbeta [(1 + beta^2) * (precision * recall) / ((beta^2 *precision) + recall)] of a classification using the confusion matrix
#' Note: Predictions should be annualized (independent of exposure)
#' Note: Perfect Fbeta is 1, poor model is 0
#'
#' @seealso \code{\link{metric_precision}}, \code{\link{metric_recall}} and \code{\link{metric_confusion_matrix}}
#'
#'
#' @section Inputs:
#' @template param-metric_classification
#' @param beta Numeric
#'
#' @return precision of classification TP / (TP + FN)
#' @export
#'
#' @examples
#'
#' metric_Fbeta(actual=c(0,1,0,0), predicted=c(0.1,0.9,0.4,0.6))
#' metric_Fbeta(actual=c(0,1,0,0), predicted=c(0.1,0.9,0.4,0.6), threshold=0.7)
#'
metric_Fbeta <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE, threshold=0.5, beta=1){

  # Check the value of beta as it is not checked in building confusion matrix
  checkmate::assert_numeric(beta, lower=0 , len=1)

  # Error checking done in metric_confusion_matrix
  confusion_mat <- metric_confusion_matrix(actual, predicted, weight, na.rm, threshold)

  if (na.rm==FALSE & any(is.na(c(actual,predicted)))){
    return(NA)
  }

  if (confusion_mat["predicted.1", "actual.1"]==0){return(0)}

  precision <- confusion_mat["predicted.1", "actual.1"] / (confusion_mat["predicted.1", "actual.1"] + confusion_mat["predicted.1", "actual.0"])
  recall <- confusion_mat["predicted.1", "actual.1"] / (confusion_mat["predicted.1", "actual.1"] + confusion_mat["predicted.0", "actual.1"])

  return((1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall))

}


#' metric_F1
#' @description
#' Returns the F1 [2 * (precision * recall) / (precision + recall)] of a classification using the confusion matrix
#' Note: Predictions should be annualized (independent of exposure)
#' Note: Perfect F1 is 1, poor model is 0
#'
#' @seealso \code{\link{metric_precision}}, \code{\link{metric_recall}} and  \code{\link{metric_Fbeta}}
#'
#'
#' @section Inputs:
#' @template param-metric_classification
#'
#' @return precision of classification TP / (TP + FN)
#' @export
#'
#' @examples
#'
#' metric_F1(actual=c(0,1,0,0), predicted=c(0.1,0.9,0.4,0.6))
#' metric_Fbeta(actual=c(0,1,0,0), predicted=c(0.1,0.9,0.4,0.6), threshold=0.7)
#'
#' ## metric_F1 is a specific value of metric_Fbeta
#' metric_Fbeta(actual=c(0,1,0,0), predicted=c(0.1,0.9,0.4,0.6), beta=1)
#'
#'
metric_F1 <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE, threshold=0.5){
  # Error checking done in metric_Fbeta
  return(metric_Fbeta(actual, predicted, weight, na.rm, threshold=threshold, beta=1))
}
