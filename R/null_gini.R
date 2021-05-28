#' null_gini
#'
#' @description
#' Return the curve for a gini curve for threshold in the prediction. Need to plot gini curve
#'
#' Note: Predictions should be annualised (independent of exposure)
#'
#' @param actual Array[Numeric] - Values we are aiming to predict.
#' @param predicted Array[Numeric] - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NUll even weighting is used
#' @param na.rm logical. Should missing values be removed?
#'
#' @return dataframe with columns
#' prediction - value of predictions (sorted)
#' cum_act - cumulative fraction of actuals
#' cum_pop - cumulative fraction of weight
#' @export
#'
#' @examples
#'
#' actual <- rnorm(100, mean=100, sd=10)
#' weight <- rep(1,100)
#' predicted <- actual + rnorm(100, mean=0, sd=1)
#'
#' null_gini(actual, predicted, weight)
#'
null_gini <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE){

  # Data prep and checking --------------------------------------------------
  checkmate::assert_numeric(actual, lower=0)
  checkmate::assert_numeric(predicted, len=length(actual))
  checkmate::assert_numeric(weight, len=length(actual), lower=0)


  # Combine actuals and predictions
  data <- data.frame(actual=actual, weight=weight, predicted=predicted)

  if (na.rm==TRUE){
    data <- data %>% dplyr::filter(!is.na(actual) & !is.na(weight) & !is.na(predicted))
  }


  # Calculate gini curve
  data_gini <- data %>%
    dplyr::group_by(predicted) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::arrange(desc(predicted)) %>%
    dplyr::transmute(predicted=predicted,
              cum_act=cumsum(actual * weight)/sum(actual * weight),
              cum_pop=cumsum(weight)/sum(weight))

  # Add leading row of 0
  data_gini <- rbind(data.frame(predicted=NA, cum_act=0, cum_pop=0), data_gini)

  return(data_gini)

}

