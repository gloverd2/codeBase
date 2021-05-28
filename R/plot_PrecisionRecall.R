#' plot_PrecisionRecall
#'
#' @description
#' Returns a plot showing the Precision-Recall curve of one or more models compared to the Null model
#'
#' #' Note: Predictions should be annualised (independent of exposure)
#'
#' @param actual Array[Numeric] - 0 or 1 - Values we are aiming to predict.
#' @param predicted Array[Numeric] / DataFrame[Numeric] - Between 0 and 1 - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used.
#' @param na.rm logical. Should missing values be removed?
#' @param use_plotly Optional: boolean - If TRUE plotly object is returned else ggplot2 object
#'
#' @seealso \code{\link{metric_PrecisionRecall}}
#'
#' @return plotly object of showing Precision-Recall curve for all predictions given
#' @export
#'
#' @examples
#'
#' data <- data.frame(x1=runif(100), x2=runif(100), noise=rnorm(100, sd=0.2)) %>%
#'   mutate(target=ifelse(x1 + noise>0.5, 1, 0))
#'
#' plot_PrecisionRecall(actual=data$target, predicted=data$x1)
#' plot_PrecisionRecall(actual=data$target, predicted=data[c("x1","x2")])
#'
plot_PrecisionRecall <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE, use_plotly=TRUE){

  checkmate::assert_numeric(actual, lower=0)
  checkmate::assert_logical(use_plotly, len=1)
  checkmate::assert_logical(na.rm, len=1)
  checkmate::assert_numeric(weight, len=length(actual), lower=0)
  checkmate::assert_logical(use_plotly, len=1)

  # Deal with predicted as dataframe or numeric vector
  if (is.numeric(predicted)){
    checkmate::assert_numeric(predicted, len=length(actual))
    predicted_df <- data.frame(predicted=predicted)
    pred_cols <- "predicted"
  }else{
    checkmate::assert_data_frame(predicted, nrows=length(actual))
    predicted_df <- predicted
    pred_cols <- colnames(predicted)
  }

  null_value <- sum(actual==1)/length(actual)

  # Build base plot
  if (use_plotly==TRUE){
    pr_plot <- plotly::plot_ly() %>%
      plotly::add_trace(x=c(0,1), y=~c(null_value, null_value), type="scatter", mode="lines", name=paste("Null", round(null_value,4), sep=" - "), line=list(dash="dot"))
  }else{
    pr_plot <- ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x=c(0,1), y=c(null_value, null_value), color=paste("NULL", round(null_value,4), sep=" - "), linetype="Null"))
  }


  # For every prediction
  for (ii in 1:ncol(predicted_df)){

    pr_df <- null_PrecisionRecall(actual=actual, predicted=predicted_df[[ii]], weight=weight, na.rm=na.rm)


    # Calculate area under the curve
    # trapezium rule - sum up. Average height ((y1+y2)/2) between two points * width between points (x2-x1)
    #pr_auc_score <- sum(((dplyr::lag(pr_df["Precision"]) + pr_df["Precision"]) * (dplyr::lag(pr_df["Recall"]) - pr_df["Recall"]))/2, na.rm=TRUE)

    pr_ave_score <- sum((pr_df["Recall"]-dplyr::lead(pr_df["Recall"])) * pr_df["Precision"], na.rm =TRUE)

    if (any(is.na(pr_df["Precision"]), is.na(pr_df["Recall"]))){
      warning("NA found in inputs. Plotting skipped")
      next
    }

    # Add curve to plot
    if (use_plotly==TRUE){
      pr_plot <- pr_plot %>%
        plotly::add_trace(data=pr_df, x=~Recall, y=~Precision, type="scatter", mode="lines", name=paste(pred_cols[ii], round(pr_ave_score,4), sep=" - "))
    }else{

      pr_df_temp <- pr_df %>%
        dplyr::arrange(Recall , desc(Precision)) %>%
        dplyr::mutate(type=paste(pred_cols[ii], round(pr_ave_score,4), sep=" - "))

      pr_plot <- pr_plot +
        ggplot2::geom_line(data=pr_df_temp, ggplot2::aes(x=Recall, y=Precision, color=type, linetype="Prediction"))

    }
  }

  #Clean plot and return
  if (use_plotly==TRUE){
    pr_plot %>% plotly::layout(xaxis=list(title="Recall", range=c(-.1, 1.1)),
                               yaxis=list(title="Precision", range=c(-.1, 1.1)),
                               legend = list(x = 0.2, y = 0.4)) %>%
      return()
  }else{
    pr_plot <- pr_plot +
      ggplot2::labs(x="Recall", y="Precision", color="Model", linetype="") +
      ggplot2::scale_linetype_manual(values =c("dashed", "solid"))

    return(pr_plot)
  }

}

