#' @param actual Array[Numeric] - Values we are aiming to predict.
#' @param predicted Array[Numeric] - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used
#' @param na.rm Optional: boolean - If \code{FALSE} function will return NA is any value in NA
#' @param threshold Optional: Numeric between 0 and 1. If prediction proablity is below \code{threshold} the predicted value is 0.
