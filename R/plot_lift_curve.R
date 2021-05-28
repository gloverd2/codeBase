#' plot_lift_curve
#'
#' @description
#' Buckets data into groups using predicted value (see \code{\link{prep_num_bin}}) and finds average predictions and actual in each bucket.
#'
#' Note: Predictions should be annualised (independent of exposure)
#'
#' @param actual Array[Numeric] - Values we are aiming to predict.
#' @param predicted Array[Numeric] - Values that we have predicted.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used
#' @param title Optional: String - Title for the plot
#' @inheritParams prep_num_bin
#' @param use_plotly Optional: boolean - If TRUE plotly object is returned else ggplot2 object
#' @param rebase boolean - If TRUE multiplicative shift is used to rebase average prediction and actuals
#'
#' @return plotly/ggplot object of showing lift curve for given predictions
#' @export
#'
#' @examples
#'
#' plot_lift_curve(actual=1:100, predicted = 1:100 + rnorm(100, mean=0, sd = 10), title="Example Lift Curve")
#' plot_lift_curve(actual=1:100, predicted = 1:100 + rnorm(100, mean=0, sd = 10), title="Example Lift Curve", method="gaussian_weight")
#'
plot_lift_curve <- function(actual,
                            predicted,
                            weight=rep(1, length(actual)),
                            title=NULL,
                            n_bins=10,
                            method="even_weight",
                            use_labels=TRUE,
                            mean=0.5,
                            sd=0.3,
                            use_plotly=TRUE,
                            rebase=FALSE){

  #Other inputs are asserted in the function prep_num_bin
  checkmate::assert_character(title, null.ok = TRUE)
  checkmate::assert_numeric(actual)
  checkmate::assert_numeric(predicted, len = length(actual))
  checkmate::assert_logical(use_plotly, len=1)
  checkmate::assert_numeric(weight, len=length(actual), lower=0)
  checkmate::assert_logical(rebase, len=1)


  # Create dataframe of output
  data <- data.frame(actual=actual, predicted=predicted, weight=weight)

  #bin data
  data$bin <- prep_num_bin(var_to_band=data$predicted, n_bins=n_bins, weight=data$weight, method=method, use_labels=use_labels, mean=mean, sd=sd)$bins

  #Summarise bins
  agg_data <- data %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(actual=sum(actual * weight)/ sum(weight)
                     ,predicted=sum(predicted * weight)/ sum(weight)
                     ,weight=sum(weight)
                     ,.groups = "keep")

  if (rebase==TRUE){
    agg_data <- agg_data %>%
      mutate(predicted=predicted * (sum(agg_data$actual * agg_data$weight)/ sum(agg_data$predicted * agg_data$weight)))
  }

  if (use_plotly==TRUE){
    plotly::plot_ly(data=agg_data) %>%
      plotly::add_trace(x=~bin, y=~actual, type="scatter", mode="line+marker", name="Actual") %>%
      plotly::add_trace(x=~bin, y=~predicted, type="scatter", mode="line+marker", name="Predicted") %>%
      plotly::add_trace(x=~bin, y=~weight, type="bar", name="Exposure", yaxis="y2", opacity=0.2) %>%
      plotly::layout(
        title = title,
        yaxis = list(title="Actual and Predicted Value", rangemode="nonnegative"),
        yaxis2 = list(overlaying = "y", side = "right", title="Exposure", showgrid = FALSE, rangemode="nonnegative"),
        xaxis = list(title="Bin")
      ) %>%
      return()
  }else{

    scale= mean(agg_data$actual + agg_data$predicted) / (2 * mean(agg_data$weight))

    out_plot <- ggplot2::ggplot(data=agg_data) +
      ggplot2::geom_point(ggplot2::aes(x=bin, y=actual, color="Actual")) +
      ggplot2::geom_line(ggplot2::aes(x=bin, y=actual, color="Actual", group=1)) +
      ggplot2::geom_point(ggplot2::aes(x=bin, y=predicted, color="Predicted")) +
      ggplot2::geom_line(ggplot2::aes(x=bin, y=predicted, color="Predicted", group=1)) +
      ggplot2::geom_col(ggplot2::aes(x=bin, y=weight * scale, fill="Exposure"), alpha=0.2) +
      ggplot2::scale_y_continuous(name="Actual and Predicted Value", sec.axis=ggplot2::sec_axis(~.*scale ,name="Exposure")) +
      ggplot2::labs(title=title, x="Bin", color="Data Type", fill="")

    if (use_labels){
      out_plot <- out_plot +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
    }

    return(out_plot)

  }

}
