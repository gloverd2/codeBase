#' plot_feature_predictions
#'
#' @description
#' function to plot the prediction vs the actual over a given feature.
#' This allows analysis of how well the predictions fit over the feature.
#' This function DOES NOT give the marginal effect of a feature, this is due to correlations with other features
#'
#' @param feature - vector of factor to be plotted over
#' @param feature_name - Name of factor to be plotted over. Will be used to label plots
#' @param actual - array[numeric] - target variable
#' @param prediction - array[numeric] - prediction of target variable
#' @param weight numeric - Vector of length \code{nrow(data)} contains weightings, if NULL even weighting is used
#' @param exposure_type -  character. either \code{'pdf'} or \code{'count'}. Method used to plot exposure
#' @param n_bins numeric - Vector of length 1 for 1D plot and 1 or 2 for 2D plots. This is the number of points to calculate the PDP for
#' @param use_plotly Optional: boolean - If TRUE plotly object is returned else ggplot2 object
#'
#' @return
#' @export
#'
#' @examples
#'
#' plot_feature_predictions(feature=rep(1:10, each = 10),
#' feature_name= "Example",
#' actual=1:100,
#' prediction=1:100 + 10 * rnorm(100))
#'
plot_feature_predictions <- function(feature, feature_name, actual, prediction, weight=rep(1, length(feature)), exposure_type="pdf", n_bins=10, use_plotly=TRUE){

  #Check inputs
  checkmate::assert_numeric(actual, len=length(feature))
  checkmate::assert_numeric(prediction, len=length(feature))
  checkmate::assert_integerish(n_bins, len = 1)
  checkmate::assert_logical(use_plotly, len=1)
  checkmate::assert_numeric(weight, len=length(feature), lower=0)
  checkmate::assert_choice(exposure_type, c("count", "pdf"))

  plot.data <- plotting_numerical_buckets(var_to_band=feature, n_bins=n_bins, weight=weight, include_outliers=TRUE)

  # For each bin
  for (ii in 1:nrow(plot.data)){

    if (ii!=nrow(plot.data)){
      feature_ii <- feature[feature >= plot.data[[ii,"lower"]] & feature < plot.data[[ii,"upper"]]]
      weight_ii <- weight[feature >= plot.data[[ii,"lower"]] & feature < plot.data[[ii,"upper"]]]
      actual_ii <- actual[feature >= plot.data[[ii,"lower"]] & feature < plot.data[[ii,"upper"]]]
      prediction_ii <- prediction[feature >= plot.data[[ii,"lower"]] & feature < plot.data[[ii,"upper"]]]
    }else{
      feature_ii <- feature[feature >= plot.data[[ii,"lower"]] & feature <= plot.data[[ii,"upper"]]]
      weight_ii <- weight[feature >= plot.data[[ii,"lower"]] & feature <= plot.data[[ii,"upper"]]]
      actual_ii <- actual[feature >= plot.data[[ii,"lower"]] & feature <= plot.data[[ii,"upper"]]]
      prediction_ii <- prediction[feature >= plot.data[[ii,"lower"]] & feature <= plot.data[[ii,"upper"]]]
    }

    plot.data[ii,"prediction"] <- sum(prediction_ii*weight_ii) / sum(weight_ii)
    plot.data[ii,"actual"] <- sum(actual_ii*weight_ii) / sum(weight_ii)
    plot.data[ii,"weight"] <- sum(weight_ii)


  }

  ### This method is slower but does the same thing
  # find_bin <- function(x){
  #   ge_lower <- plot.data$lower <= x
  #   l_upper <- x < plot.data$upper
  #   e_top_upper <- plot.data$bin == max(plot.data$bin) & (x == plot.data$upper)
  #
  #
  #   plot.data$bin[which(ge_lower & (l_upper | e_top_upper))]
  # }

  # Calculate value weight and target value in each bin
  # plot.data <- data.frame(feature=feature, prediction=prediction, actual=actual, weight=weight) %>%
  #   mutate(bin = sapply(feature, find_bin)) %>%
  #   left_join(plot.data, by="bin") %>%
  #   group_by(bin, labels, lower, upper, center, width) %>%
  #   summarise(prediction=sum(prediction*weight) / sum(weight),
  #             actual=sum(actual*weight) / sum(weight),
  #             weight=sum(weight),
  #             .groups = "drop")


  # Normalize by width if required
  if (exposure_type == "pdf"){
    plot.data[["weight"]] <- plot.data[["weight"]] / plot.data[["width"]]
  }


  if (use_plotly==TRUE){
    plotly::plot_ly(data=plot.data) %>%
      plotly::add_trace(y=~actual, x=~center, type="scatter", mode="lines+markers", name="actual") %>%
      plotly::add_trace(y=~prediction, x=~center, type="scatter", mode="lines+markers", name="prediction") %>%
      plotly::add_trace(y=~weight, x=~center, width=~width, type="bar", name="Exposure", yaxis="y2", opacity=0.2) %>%
      plotly::layout(
        title = paste0("Target vs Actual - ", feature_name),
        yaxis = list(title="Target vs Actual"),
        yaxis2 = list(overlaying = "y", side = "right", title=glue::glue("Exposure ({exposure_type})"), showgrid = FALSE, rangemode="nonnegative"),
        xaxis = list(title=feature_name)
      ) %>%
      return()
  }else{

    scale = max(mean(plot.data$actual), mean(plot.data$prediction)) / mean(plot.data$weight)

    out_plot <- ggplot2::ggplot(data=plot.data) +
      ggplot2::geom_line(ggplot2::aes(y=actual, x=center, color="actual")) +
      ggplot2::geom_point(ggplot2::aes(y=actual, x=center, color="actual")) +
      ggplot2::geom_line(ggplot2::aes(y=prediction, x=center, color="prediction")) +
      ggplot2::geom_point(ggplot2::aes(y=prediction, x=center, color="prediction")) +
      ggplot2::geom_rect(ggplot2::aes(ymax=weight  * scale, ymin=0, xmin=center-(width/2), xmax=center+(width/2), fill="Exposure"), alpha=0.2) +
      ggplot2::scale_y_continuous(name="Target vs Actual", sec.axis=ggplot2::sec_axis(~.*scale ,name=glue::glue("Exposure ({exposure_type})"))) +
      ggplot2::labs(title = paste0("Target vs Actual - ", feature_name), x=feature_name)
  }

}
