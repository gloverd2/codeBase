#' metric_confusion_matrix
#' @description
#' Returns a confusion matrix showing true(/false) positives(/negatives)
#' Note: Predictions should be annualized (independent of exposure)
#'
#'
#' @section Inputs:
#' @template param-metric_classification
#'
#' @return confusion matrix for the classification. Col names are \code{c("actual.1", "actual.0")} and Row names are c("predicted.1", "predicted.0")
#' @export
#'
#' @examples
#'
#' metric_confusion_matrix(actual=c(0,1,0,0), predicted=c(0.1,0.9,0.4,0.6))
#'
metric_confusion_matrix <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE, threshold=0.5){

  # Error catching
  metric_error_checking_classification(actual, predicted, weight, na.rm, threshold)



  # If na.rm==FALSE and there are any NAs return Na
  if (na.rm==FALSE & any(c(is.na(actual), is.na(predicted), is.na(weight)))){
    confusion_mat <- matrix(data=NA, ncol=2, nrow=2)
    colnames(confusion_mat) <- c("actual.1", "actual.0")
    rownames(confusion_mat) <- c("predicted.1", "predicted.0")
    return(confusion_mat)
  }



  df_sum <- data.frame(actual=factor(actual, levels = c(0,1)),
                       predicted=predicted,
                       weight=weight) %>%
    mutate(predicted_bol = factor(ifelse(predicted>threshold,1,0), levels = c(0, 1))) %>%
    group_by(actual, predicted_bol, .drop=FALSE) %>%
    summarise(weight=sum(weight, na.rm=TRUE), .groups="drop")


  # Create empty confusion matrix with correct col and row names
  confusion_mat <- matrix(data=0, ncol=2, nrow=2)
  colnames(confusion_mat) <- c("actual.1", "actual.0")
  rownames(confusion_mat) <- c("predicted.1", "predicted.0")

  confusion_mat["predicted.1", "actual.1"] <- df_sum %>% filter(actual==1 & predicted_bol==1) %>% pull(weight)
  confusion_mat["predicted.1", "actual.0"] <- df_sum %>% filter(actual==0 & predicted_bol==1) %>% pull(weight)
  confusion_mat["predicted.0", "actual.1"] <- df_sum %>% filter(actual==1 & predicted_bol==0) %>% pull(weight)
  confusion_mat["predicted.0", "actual.0"] <- df_sum %>% filter(actual==0 & predicted_bol==0) %>% pull(weight)


  return(confusion_mat)

}

