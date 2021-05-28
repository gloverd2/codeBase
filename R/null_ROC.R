#' null_ROC
#'
#' @description
#' Return the true positive rate and false positive rate for each threshold in the prediction. Need to plot ROC curve
#'
#' @param actual Array[Numeric] - 0 or 1 - Values we are aiming to predict.
#' @param predicted Array[Numeric] / DataFrame[Numeric] - Between 0 and 1 - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used
#' @param na.rm logical. Should missing values be removed?
#'
#' @seealso \code{\link{plot_ROC}} and \code{\link{metric_ROC}}
#'
#' @return dataframe with columns
#' Threshold - value of the threshold
#' TP_Rate - True positive rate
#' FP_Rate - False positive rate
#' @export
#'
#' @examples
#'
#' data <- data.frame(x1=runif(100), x2=runif(100), noise=rnorm(100, sd=0.2)) %>%
#'   mutate(target=ifelse(x1 + noise>0.5, 1, 0))
#'
#' null_ROC(actual=data$target, predicted=data$x1)
null_ROC <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE){
  # Checking weight
  checkmate::assert_numeric(weight, len=length(actual), lower=0)


  checkmate::assert_numeric(predicted, len=length(actual))

  # Build empty dataframe to hold results
  # Find all the threshold cuts and use if this value is small
  thresholds_all <- c(0, predicted %>% unique %>% sort(), 1)
  if (length(thresholds_all)<250){
    roc_df <- data.frame(Threshold=c(NA,thresholds_all,NA))
  }else{
    roc_df <- data.frame(Threshold=c(NA,seq(0,1,length.out = 250),NA))
  }




  roc_df[1,"TP_Rate"] <- 1
  roc_df[1,"FP_Rate"] <- 1
  roc_df[nrow(roc_df),"TP_Rate"] <- 0
  roc_df[nrow(roc_df),"FP_Rate"] <- 0

  # Find the true positive and false positive rate at each threshold
  for (jj in 2:(nrow(roc_df)-1)){
    conf_mat <- metric_confusion_matrix(actual=actual, predicted=predicted, weight=weight, threshold = roc_df[jj, "Threshold"], na.rm=na.rm)

    roc_df[jj,"TP_Rate"] <- conf_mat["predicted.1", "actual.1"] / (conf_mat["predicted.1", "actual.1"] + conf_mat["predicted.0", "actual.1"])
    roc_df[jj,"FP_Rate"] <- conf_mat["predicted.1", "actual.0"] / (conf_mat["predicted.1", "actual.0"] + conf_mat["predicted.0", "actual.0"])
  }

  return(roc_df)
}
