#' @param actual Array[Numeric] - Values we are aiming to predict.
#' @param predicted Array[Numeric] - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used
#' @param na.rm Optional: boolean - If \code{FALSE} function will return NA is any value in NA
#' @param rebase Optional: boolean - If \code{TRUE} predictions weighted mean will be rebased to match actual mean
