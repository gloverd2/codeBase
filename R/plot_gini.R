#' plot_gini
#'
#' @description
#' Returns a plot showing the gini of one or more models compared to the perfect model and the Null model
#'
#' Note: Predictions should be annualised (independent of exposure)
#'
#' @param actual Array[Numeric] - Values we are aiming to predict.
#' @param predicted Array[Numeric] / DataFrame[Numeric] - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used
#' @param na.rm logical. Should missing values be removed?
#' @param use_plotly Optional: boolean - If TRUE plotly object is returned else ggplot2 object
#'
#' @seealso \code{\link{metric_gini}}
#'
#' @return plotly/ggplot object of showing gini curve for all predictions given
#' @export
#'
#' @examples
#'
#' # Input as vector
#' actual <- rnorm(100, mean=100, sd=10)
#' weight <- rep(1,100)
#' predicted <- actual + rnorm(100, mean=0, sd=1)
#'
#' plot_gini(actual, predicted, weight)
#'
#' # Input as dataframe
#'
#' predicted <- data.frame(pred1 = actual + rnorm(100, mean=0, sd=1), pred2 = rnorm(100, mean=0, sd=1))
#' plot_gini(actual, predicted, weight)
#'
plot_gini <- function(actual, predicted, weight=rep(1, length(actual)), na.rm=FALSE, use_plotly=TRUE){

  # Data prep and checking --------------------------------------------------
  checkmate::assert_numeric(actual, lower=0)
  checkmate::assert_logical(na.rm, len=1)
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

  # Combine actuals and predictions
  data <- data.frame(actual=actual, weight=weight) %>%
    cbind(predicted_df)


  # Ideal GINI --------------------------------------------------------------

  data_t_act <- null_gini(actual=data[["actual"]], predicted=data[["actual"]], weight=data[["weight"]], na.rm=na.rm)



  # Plot ideal gini and null gini
  if (use_plotly==TRUE){
    out_plot <- plotly::plot_ly() %>%
      plotly::add_trace(data=data_t_act, x=~cum_pop, y=~cum_act, type="scatter", mode="lines", name="Perfect Gini", line=list(color="black", dash="dash")) %>%
      plotly::add_trace(data=data_t_act, x=~cum_act, y=~cum_act, type="scatter", mode="lines", name="Null Gini", line=list(color="black", dash="dot"))
  }else{
    out_plot <- ggplot2::ggplot(data=data_t_act) +
      ggplot2::geom_line(ggplot2::aes(x=cum_pop, y=cum_act, linetype="Perfect Gini")) +
      ggplot2::geom_line(ggplot2::aes(x=cum_act, y=cum_act, linetype="Null Gini"))
  }


  # Calculate ideal AUC coefficent
  # trapezium rule for area under a curve then subtract 0.5 for area above y=x line
  # trapezium rule - sum up. Average height ((y1+y2)/2) between two points * width between points (x2-x1)
  AUC_ideal <- sum(((lag(data_t_act[["cum_act"]]) + data_t_act[["cum_act"]]) * (data_t_act[["cum_pop"]] - lag(data_t_act[["cum_pop"]])))/2, na.rm=TRUE) - 0.5

  # Prediction GINI --------------------------------------------------------------

  # Cacluate and plot gini for each prediction column
  for (pred_col in pred_cols){

    # Calculate curve
    data_t_pred <- null_gini(actual=data[["actual"]], predicted=data[[pred_col]], weight=data[["weight"]], na.rm=na.rm)

    # Calculate area under curve
    # trapezium rule for area under a curve then subtract 0.5 for area above y=x line
    # trapezium rule - sum up. Average height ((y1+y2)/2) between two points * width between points (x2-x1)
    AUC_pred <- sum(((lag(data_t_pred[["cum_act"]]) + data_t_pred[["cum_act"]]) * (data_t_pred[["cum_pop"]] - lag(data_t_pred[["cum_pop"]])))/2, na.rm=TRUE) - 0.5
    # normalised to caluculate gini
    gini <- AUC_pred/AUC_ideal

    if (any(is.na(data_t_pred["cum_act"]), is.na(data_t_pred["cum_act"]))){
      warning("NA found in inputs. Plotting skipped")
      next
    }

    # Plot prediction gini
    if (use_plotly==TRUE){
      out_plot <- out_plot %>%
        plotly::add_trace(data=data_t_pred, x=~cum_pop, y=~cum_act, type="scatter", mode="lines", name=paste(pred_col, round(gini,4), sep=" - "))
    }else{

      out_plot <- out_plot +
        ggplot2::geom_line(data=data_t_pred %>% dplyr::mutate(type=paste(pred_col, round(gini,4), sep=" - ")),
                           ggplot2::aes(x=cum_pop, y=cum_act, color=type, linetype="Prediction"))
    }

  }

  # Clean plot and return
  if (use_plotly==TRUE){
    out_plot %>%
      plotly::layout(legend=list(x=0.6, y=0.4),
             xaxis=list(rangemode="tozero", title="Cumulative Population"),
             yaxis=list(rangemode="tozero", title="Cumulative Response")) %>%
    return()
  }else{
    out_plot <- out_plot +
      ggplot2::labs(x="Cumulative Population", y="Cumulative Response", linetype="Type", color="Prediction") +
      ggplot2::scale_linetype_manual(breaks=c("Prediction", "Perfect Gini", "Null Gini"), values=c("solid","twodash", "dotted")) +
      ggplot2::theme(
        legend.position = c(.95, .6),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = ggplot2::margin(6, 6, 6, 6)
      )

    return(out_plot)
  }

}
