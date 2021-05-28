#' plot_lift_curve_relative
#'
#' @description
#' Buckets data into groups using the difference in predicted values (\code{proposed_pred} - \code{incumbent_pred}) (see \code{\link{prep_num_bin}}) and finds average predictions and actual in each bucket.
#'
#' Note: Predictions should be annualised (independent of exposure)
#'
#' @param actual Array[Numeric] - Values we are aiming to predict.
#' @param incumbent_pred Array[Numeric] - Values that we have predicted by the incumbent model. Incumbent is the current model looking to be replaced.
#' @param proposed_pred Array[Numeric] - Values that we have predicted by the proposed model. Proposed is the new model looking to be replace the incumbent.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used.
#' @param title Optional: String - Title for the plot.
#' @inheritParams prep_num_bin
#' @param incumbent_label Optional: String - Text to use to label the incumbent predictions.
#' @param proposed_pred Optional: String - Text to use to label the proposed predictions.
#' @param use_plotly Optional: boolean - If TRUE plotly object is returned else ggplot2 object.
#' @param rebase boolean - If TRUE multiplicative shift is used to rebase average prediction and actuals
#'
#' @return plotly/ggplot object of showing relative lift curve for given pair of predictions
#' @export
#'
#' @examples
#'
#' plot_lift_curve_relative(actual=1:100, incumbent_pred = seq(25,74.5,0.5) + rnorm(100, mean=0, sd = 10), proposed_pred=seq(1,100,1) + rnorm(100, mean=0, sd = 10), title="Example Lift Curve")
#' plot_lift_curve_relative(actual=1:100, incumbent_pred = seq(25,74.5,0.5) + rnorm(100, mean=0, sd = 10), proposed_pred=seq(1,100,1) + rnorm(100, mean=0, sd = 10), title="Example Lift Curve", method="gaussian_weight")
#' plot_lift_curve_relative(actual=1:100, incumbent_pred = seq(25,74.5,0.5) + rnorm(100, mean=0, sd = 10), proposed_pred=seq(1,100,1) + rnorm(100, mean=0, sd = 10), title="Example Lift Curve", use_plotly=FALSE)
#' plot_lift_curve_relative(actual=1:100, incumbent_pred = seq(25,74.5,0.5) + rnorm(100, mean=0, sd = 10), proposed_pred=seq(1,100,1) + rnorm(100, mean=0, sd = 10), title="Example Lift Curve", method="gaussian_weight", use_plotly=FALSE)
#'
plot_lift_curve_relative <- function(actual,
                                     incumbent_pred,
                                     proposed_pred,
                                     weight=rep(1, length(actual)),
                                     title=NULL,
                                     n_bins=10,
                                     method="even_weight",
                                     use_labels=TRUE,
                                     mean=0.5,
                                     sd=0.3,
                                     incumbent_label="Incumbent",
                                     proposed_label="Proposed",
                                     use_plotly=TRUE,
                                     rebase=FALSE){

  #Other inputs are asserted in the function prep_num_bin
  checkmate::assert_character(title, null.ok = TRUE)

  checkmate::assert_numeric(actual, any.missing = FALSE)

  checkmate::assert_numeric(incumbent_pred, len = length(actual), any.missing = FALSE)
  checkmate::assert_character(incumbent_label)

  checkmate::assert_numeric(proposed_pred, len = length(actual), any.missing = FALSE)
  checkmate::assert_character(proposed_label)

  checkmate::assert_logical(use_plotly, len=1)
  checkmate::assert_logical(rebase, len=1)

  checkmate::assert_numeric(weight, len=length(actual), lower=0, any.missing = FALSE)



  # Create dataframe of output
  data <- data.frame(actual=actual, incumbent_pred=incumbent_pred, proposed_pred=proposed_pred, weight=weight) %>%
    dplyr::mutate(pred_diff = proposed_pred-incumbent_pred)

  # rebase if required
  if (rebase==TRUE){
    data <- data %>%
      dplyr::mutate(incumbent_pred=incumbent_pred * (sum(data$actual * data$weight)/ sum(data$incumbent_pred * data$weight)),
                    proposed_pred=proposed_pred * (sum(data$actual * data$weight)/ sum(data$proposed_pred * data$weight)),
                    pred_diff = proposed_pred-incumbent_pred)
  }

  #bin data
  data$bin <- prep_num_bin(var_to_band=data$pred_diff, n_bins=n_bins, weight=data$weight, method=method, use_labels=use_labels, mean=mean, sd=sd)$bins

  #Summarise bins
  agg_data <- data %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(actual=sum(actual * weight)/ sum(weight)
                     ,incumbent_pred=sum(incumbent_pred * weight)/ sum(weight)
                     ,proposed_pred=sum(proposed_pred * weight)/ sum(weight)
                     ,weight=sum(weight)
                     ,.groups = "keep")


  # Calculate distance between actuals and predictions. Useful to put on plots
  incumbent_mae <- metric_mae(actual=agg_data$actual, predicted=agg_data$incumbent_pred, weight=agg_data$weight)
  proposed_mae <- metric_mae(actual=agg_data$actual, predicted=agg_data$proposed_pred, weight=agg_data$weight)


  if (use_plotly==TRUE){

    plotly::plot_ly(data=agg_data) %>%
      plotly::add_trace(x=~bin, y=~actual, type="scatter", mode="line+marker", name="Actual") %>%
      plotly::add_trace(x=~bin, y=~incumbent_pred, type="scatter", mode="line+marker", name=paste0(incumbent_label, "- mae: ", signif(incumbent_mae, digits = 2))) %>%
      plotly::add_trace(x=~bin, y=~proposed_pred, type="scatter", mode="line+marker", name=paste0(proposed_label, " - mae: ", signif(proposed_mae, digits = 2))) %>%
      plotly::add_trace(x=~bin, y=~weight, type="bar", name="Exposure", yaxis="y2", opacity=0.2) %>%
      plotly::layout(
        title = title,
        yaxis = list(title="Actual and Predicted Value", rangemode="nonnegative"),
        yaxis2 = list(overlaying = "y", side = "right", title="Exposure", showgrid = FALSE, rangemode="nonnegative"),
        xaxis = list(title=paste0("Bin (", proposed_label, "-", incumbent_label, ")"))
      ) %>%
      return()
  }else{
    scale= mean(agg_data$actual + agg_data$incumbent_pred + agg_data$proposed_pred) / (3 * mean(agg_data$weight))

    out_plot <- ggplot2::ggplot(data=agg_data) +
      ggplot2::geom_point(ggplot2::aes(x=bin, y=actual, color="Actual")) +
      ggplot2::geom_line(ggplot2::aes(x=bin, y=actual, color="Actual", group=1)) +
      ggplot2::geom_point(ggplot2::aes(x=bin, y=incumbent_pred, color=paste0(incumbent_label, " :  - mae: ", signif(incumbent_mae, digits = 2)))) +
      ggplot2::geom_line(ggplot2::aes(x=bin, y=incumbent_pred, color=paste0(incumbent_label, " :  - mae: ", signif(incumbent_mae, digits = 2)), group=1)) +
      ggplot2::geom_point(ggplot2::aes(x=bin, y=proposed_pred, color=paste0(proposed_label, " :  - mae: ", signif(proposed_mae, digits = 2)))) +
      ggplot2::geom_line(ggplot2::aes(x=bin, y=proposed_pred, color=paste0(proposed_label, " :  - mae: ", signif(proposed_mae, digits = 2)), group=1)) +
      ggplot2::geom_col(ggplot2::aes(x=bin, y=weight * scale, fill="Exposure"), alpha=0.2) +
      ggplot2::scale_y_continuous(name="Actual and Predicted Value", sec.axis=ggplot2::sec_axis(~.*scale ,name="Exposure")) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(title=title, x="Bin", color="Data Type", fill="")


    if (use_labels){
      out_plot <- out_plot +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
    }

    return(out_plot)
  }

}
