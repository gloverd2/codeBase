#' null_PrecisionRecall
#'
#' @description
#' Return the precision and recall for each threshold in the prediction. Need to plot Precision-Recall curve
#'
#' @param actual Array[Numeric] - 0 or 1 - Values we are aiming to predict.
#' @param predicted Array[Numeric] - Between 0 and 1 - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used.
#' @param na.rm logical. Should missing values be removed?
#'
#' @seealso \code{\link{plot_PrecisionRecall}} and \code{\link{metric_PrecisionRecall}}
#'
#' @return dataframe with columns
#' Threshold - value of the threshold
#' Precision - Precision at each threshold
#' Recall - Recall at each threshold
#' @export
#'
#' @examples
#'
#' data <- data.frame(x1=runif(100), x2=runif(100), noise=rnorm(100, sd=0.2)) %>%
#'   mutate(target=ifelse(x1 + noise>0.5, 1, 0))
#'
#' null_PrecisionRecall(actual=data$target, predicted=data$x1)
null_PrecisionRecall <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE){
  # Checking weight
  checkmate::assert_numeric(weight, len=length(actual), lower=0)


  checkmate::assert_numeric(predicted, len=length(actual))


  # Build empty dataframe to hold results
  # Find all the threshold cuts and use if this value is small
  thresholds_all <- c(0, predicted %>% unique %>% sort(), 1)
  if (length(thresholds_all)<250){
    pr_df <- data.frame(Threshold=c(NA,thresholds_all,NA))
  }else{
    pr_df <- data.frame(Threshold=c(NA,seq(0,1,length.out = 250),NA))
  }

  # Populate limits
  pr_df[1,"Precision"] <- sum(weight[actual==1], na.rm=na.rm)/sum(weight, na.rm=na.rm)
  pr_df[1,"Recall"] <- 1

  pr_df[nrow(pr_df),"Precision"] <- 1
  pr_df[nrow(pr_df),"Recall"] <- 0

  if (any(is.na(actual), is.na(predicted), is.na(weight)) & na.rm==FALSE){
    return(pr_df)
  }

  # Find the true positive and false positive rate at each threshold
  for (jj in 2:(nrow(pr_df)-1)){
    conf_mat <- metric_confusion_matrix(actual=actual, predicted=predicted, weight=weight, threshold = pr_df[jj, "Threshold"], na.rm=na.rm)

    if ((conf_mat["predicted.1", "actual.1"] + conf_mat["predicted.1", "actual.0"])==0){
      pr_df[jj,"Precision"]=1
      pr_df[jj,"Recall"] <- 0
    }else{
      pr_df[jj,"Precision"] <- conf_mat["predicted.1", "actual.1"] / (conf_mat["predicted.1", "actual.1"] + conf_mat["predicted.1", "actual.0"])
      pr_df[jj,"Recall"] <- conf_mat["predicted.1", "actual.1"] / (conf_mat["predicted.1", "actual.1"] + conf_mat["predicted.0", "actual.1"])
    }

  }
  return(pr_df)
}
