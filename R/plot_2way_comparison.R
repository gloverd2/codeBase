#' plot_2way_comparison
#'
#' @description
#' plot to inform which of two models is closer to the actuals. This is done by faceting one score by the other and inspecting if the trend is flat
#'
#' @param actual Array[Numeric] - Values we are aiming to predict.
#' @param incumbent_pred Array[Numeric] - Values that we have predicted by the incumbent model. Incumbent is the current model looking to be replaced.
#' @param proposed_pred Array[Numeric] - Values that we have predicted by the proposed model. Proposed is the new model looking to be replace the incumbent.
#' @param weight Optional: Array[Numeric] - Weighting of predictions. If NULL even weighting is used.
#' @param n_bins integerish: number bins to split data into. Works best if n_bins is square number
#' @param incumbent_label Optional: String - Text to use to label the incumbent predictions.
#' @param proposed_pred Optional: String - Text to use to label the proposed predictions.
#' @param min_weight Optional: Numeric -minimum value of weight to be included in actual. Raise to remove volatility in "actuals" seen in plots
#' @param rebase Optional: logical - should the scores be rebased to best match the actuals
#'
#' @return a list of 2 ggplot objects. Each of these plots shows one score split by the other.
#'
#'
#' @export
#'
#' @examples
#'
#' actual <- rnorm(1000)
#' incumbent_pred <- actual + (rnorm(1000) * 0.1)
#' proposed_pred <- actual + rnorm(1000)
#'
#' plot_2way_comparison(actual=actual,
#'                      incumbent_pred=incumbent_pred,
#'                      proposed_pred=proposed_pred)
#'
plot_2way_comparison <- function(actual,
                                 incumbent_pred,
                                 proposed_pred,
                                 weight=rep(1, length(actual)),
                                 n_bins=9,
                                 incumbent_label="Incumbent", proposed_label="Proposed",
                                 min_weight=0,
                                 rebase=FALSE){

  checkmate::assert_numeric(actual, any.missing = FALSE)

  checkmate::assert_numeric(incumbent_pred, len = length(actual), any.missing = FALSE)
  checkmate::assert_character(incumbent_label)

  checkmate::assert_numeric(proposed_pred, len = length(actual), any.missing = FALSE)
  checkmate::assert_character(proposed_label)

  checkmate::assert_numeric(weight, len = length(actual), lower=0, any.missing = FALSE)

  checkmate::assert_integerish(n_bins, len=1, lower=2, upper=6^2, any.missing = FALSE)
  if (round(n_bins^0.5)!=n_bins^0.5){
    warning("function plot_2way_comparison works best with n_bins as a square number")
  }

  checkmate::assert_numeric(min_weight, len = 1, lower=0, upper=sum(weight), any.missing = FALSE)

  checkmate::assert_logical(rebase, len=1, any.missing = FALSE)

  # Create a dataframe splitting target and incumbent into bins
  binned_pred <- data.frame(actual=actual,
                   incumbent_pred=incumbent_pred,
                   proposed_pred=proposed_pred,
                   weight=weight,
                   #bin predictions
                   pred_incumbent_bin=prep_num_bin(var_to_band=incumbent_pred, n_bins=n_bins, weight=weight, use_labels = FALSE)$bin %>% as.factor(),
                   pred_proposed_bin=prep_num_bin(var_to_band=proposed_pred, n_bins=n_bins, weight=weight, use_labels = FALSE)$bin %>% as.factor()
                   )


  # Group and summarize actuals by both bins, and then predictions by each bin independently
  binned_pred_sum <- binned_pred %>%
    dplyr::group_by(pred_incumbent_bin, pred_proposed_bin) %>%
    dplyr::summarise(mean_actual=sum(actual*weight)/sum(weight),
                      weight=sum(weight),
                      .groups = "keep") %>%
    dplyr::left_join(binned_pred %>%
                       group_by(pred_incumbent_bin) %>%
                       summarise(mean_incumbent = sum(incumbent_pred*weight)/sum(weight), .groups = "keep"),
                     by="pred_incumbent_bin") %>%
    dplyr::left_join(binned_pred %>%
                       group_by(pred_proposed_bin) %>%
                      summarise(mean_proposed = sum(proposed_pred*weight)/sum(weight), .groups = "keep"),
                    by="pred_proposed_bin") %>%
    ungroup()

  # rebase if required
  if (rebase==TRUE){
    binned_pred_sum <- binned_pred_sum %>%
      mutate(mean_incumbent=mean_incumbent * (sum(mean_actual*weight)/sum(mean_incumbent*weight)),
             mean_proposed=mean_proposed * (sum(mean_actual*weight)/sum(mean_proposed*weight)))
  }

  # Filter data to remove rows (really actuals) with very low weight as these are volitile
  binned_pred_sum_filter <- binned_pred_sum %>%
    filter(weight > min_weight)

  # Give error if filter is too strong and all data is removed
  if (nrow(binned_pred_sum_filter)==0){
    stop("in function plot_2way_comparison min_weight is too high and has removed all the data")
  }

  # Calculate distance between actuals and predictions. Useful to put on plots
  incumbent_mae <- metric_mae(actual=binned_pred_sum$mean_actual, predicted=binned_pred_sum$mean_incumbent, weight=binned_pred_sum$weight)
  proposed_mae <- metric_mae(actual=binned_pred_sum$mean_actual, predicted=binned_pred_sum$mean_proposed, weight=binned_pred_sum$weight)

  #produce the plot: Use one function called twice to ensure symmetry
  out_plot <- function(x_col, facet_col, x_string, facet_string){
    scale <- max(binned_pred_sum_filter$mean_actual) / max(binned_pred_sum$weight)


      ggplot2::ggplot(data= binned_pred_sum_filter, ggplot2::aes_string(x=x_col, y="mean_actual", color=facet_col, fill=facet_col, group=facet_col)) +
        ggplot2::geom_point() +
        ggplot2::geom_line(ggplot2::aes(linetype="Actual")) +
        ggplot2::geom_line(data= binned_pred_sum, ggplot2::aes(y=mean_incumbent, linetype=paste0(incumbent_label, " - mae: ", signif(incumbent_mae, digits = 2)))) +
        ggplot2::geom_line(data= binned_pred_sum, ggplot2::aes(y=mean_proposed, linetype=paste0(proposed_label, " - mae: ", signif(proposed_mae, digits = 2)))) +
        ggplot2::geom_col(data= binned_pred_sum, ggplot2::aes(y=weight * scale), alpha=0.1) +
        ggplot2::scale_y_continuous(name="Actual Value", sec.axis=ggplot2::sec_axis(~. / scale ,name="Exposure")) +
        ggplot2::facet_wrap(as.formula(paste0("~", facet_col))) +
        ggplot2::labs(title = glue::glue("{x_string} split by {facet_string}"),
             x=paste0(x_string, " bin"),
             color=paste0(facet_string, " bin"),
             fill=paste0(facet_string, " bin"),
             linetype="") +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(linetype=ggplot2::guide_legend(nrow=3))
  }

  #produce the same plot twice, splitting each score by the other
  list(
    out_plot(x_col="pred_incumbent_bin", facet_col="pred_proposed_bin", x_string=incumbent_label, facet_string=proposed_label),
    out_plot(x_col="pred_proposed_bin", facet_col="pred_incumbent_bin", x_string=proposed_label, facet_string=incumbent_label)
  ) %>%
    return()
}



