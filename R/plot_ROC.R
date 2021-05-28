#' plot_ROC
#'
#' @description
#' Returns a plot showing the ROC curve of one or more models compared to the null model
#'
#' #' Note: Predictions should be annualised (independent of exposure)
#'
#' @param actual Array[Numeric] - 0 or 1 - Values we are aiming to predict.
#' @param predicted Array[Numeric] / DataFrame[Numeric] - Between 0 and 1 - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used.
#' @param na.rm logical. Should missing values be removed?
#' @param use_plotly Optional: boolean - If TRUE plotly object is returned else ggplot2 object
#'
#'
#' @return plotly object of showing ROC curve for all predictions given
#' @export
#'
#' @examples
#'
#' data <- data.frame(x1=runif(100), x2=runif(100), noise=rnorm(100, sd=0.2)) %>%
#'   mutate(target=ifelse(x1 + noise>0.5, 1, 0))
#'
#' plot_ROC(actual=data$target, predicted=data$x1)
#' plot_ROC(actual=data$target, predicted=data[c("x1","x2")])
#'
plot_ROC <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE, use_plotly=TRUE){

  checkmate::assert_numeric(actual, lower=0)
  checkmate::assert_logical(use_plotly, len=1)
  checkmate::assert_numeric(weight, len=length(actual), lower=0)


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

  # Build base plot
  if(use_plotly==TRUE){
    roc_plot <- plotly::plot_ly() %>%
      plotly::add_trace(x=c(0,1), y=c(0,1), type="scatter", mode="lines", name="NULL - 0.5", line=list(dash="dot"))
  }else{
    roc_plot <- ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x=c(0,1), y=c(0,1), linetype="Null", color="NULL - 0.5"))
  }

  # For every prediction
  for (ii in 1:ncol(predicted_df)){

    # Build empty dataframe to hold results
    roc_df <- null_ROC(actual=actual, predicted=predicted_df[[ii]], weight=weight, na.rm=na.rm)

    # Calculate area under the curve
    # trapezium rule for area under a curve then subtract 0.5 for area above y=x line
    # trapezium rule - sum up. Average height ((y1+y2)/2) between two points * width between points (x2-x1)
    roc_auc_score <- sum(((dplyr::lag(roc_df["TP_Rate"]) + roc_df["TP_Rate"]) * (dplyr::lag(roc_df["FP_Rate"]) - roc_df["FP_Rate"]))/2, na.rm=TRUE)


    if (any(is.na(roc_df["TP_Rate"]), is.na(roc_df["FP_Rate"]))){
      warning("NA found in inputs. Plotting skipped")
      next
    }


    # Add curve to plot
    if(use_plotly==TRUE){
      roc_plot <- roc_plot %>%
        plotly::add_trace(data=roc_df, x=~FP_Rate, y=~TP_Rate, type="scatter", mode="lines", name=paste(pred_cols[ii], round(roc_auc_score,4), sep=" - "))
    }else{
      roc_df_temp <- roc_df %>%
        dplyr::arrange(TP_Rate, desc(FP_Rate)) %>%
        dplyr::mutate(type=paste(colnames(predicted_df)[ii], round(roc_auc_score,4), sep=" - "))

      roc_plot <- roc_plot +
        ggplot2::geom_line(data=roc_df_temp, ggplot2::aes(x=FP_Rate, y=TP_Rate, color=type, linetype="Prediction"))
    }
  }

  if(use_plotly==TRUE){
    #Clean plot and return
    roc_plot %>% plotly::layout(xaxis=list(title="False Positive Rate", range=c(-.01,1.01)),
                                yaxis=list(title="True Positive Rate", range=c(-.01,1.01)),
                                legend = list(x = 0.6, y = 0.4)) %>%
      return()
  }else{
    roc_plot <- roc_plot +
      ggplot2::labs(x="False Positive Rate", y="True Positive Rate", color="Model", linetype="") +
      ggplot2::scale_linetype_manual(values =c("dashed", "solid"))

    return(roc_plot)
  }
}

