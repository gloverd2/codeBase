#' plot_magic_carpet
#'
#' @description
#' This plot visually compares how predictions change over a factor.
#' The ratio of two predictions \code{proposed_pred/incumbent_pred} is calculated for each row. Two plots are produced which display
#' For each level of the feature we find what is the proportion of row with each ratio
#' For each value of the ratio what is the proportion of row with each value of the feature
#' The log2 is used to calculate the ratio as this is symmetrical
#' the values of \code{log2(proposed_pred/incumbent_pred)} which are plotted are -ratio_max, -(n * ratio_step), -((n-1) * ratio_step), ..., -ratio_step, 0, ratio_step, ..., (n-1) * ratio_step, n * ratio_step, ratio_max
#'
#'
#' @param feature array[numeric/character/factor] - value of feature
#' @param feature_name character - name of feature
#' @param incumbent_pred array[numeric] - values of incumbent prediction
#' @param proposed_pred array[numeric] - values of proposed prediction
#' @param weight array[numeric] - weight of each row
#' @param n_bins intergerish - number of buckets to split each (numeric) feature into
#' @param ratio_max numeric - max ratio at which to cap the incumbent_pred/proposed_pred ratio
#' @param ratio_step numeric - step size to divide ratio bins.
#' @param position character - either \code{"fill"} or \code{"stack"}.
#' If \code{"fill"} all bars are the same hight and extra line is added to show relative population.
#' If \code{"stack"} bar hight gives population.
#'
#' @return
#' @export
#'
#' @examples
#' n=100
#' feature1 <- seq(-10, 10, length.out=n)
#' feature2 <- rep(c("a", "b", "c"), each=ceiling(n/3))[1:n]
#' incumbent_pred <- 100 + rnorm(n)
#' proposed_pred <- 100 + rnorm(n) + seq(-10, 10, length.out=n)
#'
#' plot_magic_carpet(feature=feature1, feature_name="example feature",
#'                   incumbent_pred = incumbent_pred,
#'                   proposed_pred = proposed_pred)
#'
#' plot_magic_carpet(feature=feature2, feature_name="example feature",
#'                   incumbent_pred = incumbent_pred,
#'                   proposed_pred = proposed_pred)
#'
plot_magic_carpet <- function(feature,
                              feature_name,
                              incumbent_pred,
                              proposed_pred,
                              weight=rep(1, length(feature)),
                              n_bins=10,
                              ratio_max=1,
                              ratio_step=0.05,
                              position="fill"){

  checkmate::assert_vector(feature, any.missing = FALSE)
  checkmate::assert_character(feature_name, len=1, any.missing = FALSE)

  checkmate::assert_numeric(incumbent_pred, len=length(feature), lower=0, finite = TRUE, any.missing = FALSE)
  checkmate::assert_numeric(proposed_pred, len=length(feature), lower=0, finite = TRUE, any.missing = FALSE)
  checkmate::assert_numeric(weight, len=length(feature), finite = TRUE, lower=0, any.missing = FALSE)

  checkmate::assert_integerish(n_bins, len=1, lower=2, upper=20, any.missing = FALSE)
  checkmate::assert_numeric(ratio_max, len=1, lower=0, any.missing = FALSE)
  checkmate::assert_numeric(ratio_step, len=1, lower=0, upper=ratio_max, any.missing = FALSE)

  checkmate::assert_choice(position, choices = c("fill", "stack"))

  # Put data into a dataframe
  df <- data.frame(feature=feature,
                   incumbent_pred=incumbent_pred,
                   proposed_pred=proposed_pred,
                   weight=weight) %>%
    mutate(ratio=(proposed_pred/incumbent_pred), # create ratio
           log2_ratio_raw=log2(ratio), # Take log of ratio to make it symetric
           log2_ratio_round=(round(log2_ratio_raw / ratio_step) * ratio_step) %>% round(digits = 6), # bucket ratio
           log2_ratio_cap=pmax(-ratio_max ,pmin(ratio_max, log2_ratio_round))) # cap buckets

  # Calculate all allowed values in bounds
  ratio_possible_all <- c(min(df$log2_ratio_cap),
                        seq(0, ratio_max, ratio_step),
                        -seq(0, ratio_max, ratio_step),
                        max(df$log2_ratio_cap)) %>%
                      unique() %>%
                      sort() %>%
                      round(digits = 6)
  ratio_possible <- ratio_possible_all[ratio_possible_all>=min(df$log2_ratio_cap) & ratio_possible_all<=max(df$log2_ratio_cap)]

  #Create factor containing ratio.
  #two vesions are different labels
  # df$log2_ratio_string <- factor(df$log2_ratio_cap, levels=ratio_possible, labels = ratio_possible, ordered = TRUE)
  df$log2_ratio_string <- factor(df$log2_ratio_cap, levels=ratio_possible, labels = paste0("2^",ratio_possible), ordered = TRUE)

  # Bin feature
  if (is.numeric(feature) & !(feature %>% unique() %>% length() < 2*n_bins)){
    df$feature_bin <- prep_num_bin(var_to_band=feature, n_bins=n_bins, weight=weight, use_labels = TRUE)$bin %>% as.factor()
  }else{
    df$feature_bin <- factor(df$feature)
  }


  # Find weight of each feature capped at different levels
  df_sum <- df %>%
    group_by(feature_bin, log2_ratio_string, log2_ratio_cap) %>%
    summarise(weight=sum(weight), .groups="keep")

  # Create pretty color scale
  # 0 is white. The more negative the value the more red. The more positive the more blue
  temp_fill_scale <- c(colorRampPalette(c("blue", "white"))(sum(ratio_possible<=0))[1:(sum(ratio_possible<=0)-1)],
                       "#FFFFFF",
                       colorRampPalette(c("white", "red"))(sum(ratio_possible>=0))[2:sum(ratio_possible>=0)])

  # Use to highlight value at 0
  temp_color_scale <- c(rep("white", sum(ratio_possible<0)),
                        rep("black", sum(ratio_possible==0)),
                        rep("white", sum(ratio_possible>0)))

  temp_size_scale <- c(rep(0, sum(ratio_possible<0)),
                       rep(1, sum(ratio_possible==0)),
                       rep(0, sum(ratio_possible>0)))

  # create empty output for plots
  out_plot <- list()

  #Create plots: two options stacked or fill.
  if (position=="stack"){
    out_plot[["feature_by_ratio"]] <-
      ggplot2::ggplot(data=df_sum) +
      ggplot2::geom_col(ggplot2::aes(x=feature_bin, y=weight, fill=log2_ratio_string, color=log2_ratio_string, size=log2_ratio_string), position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::scale_fill_manual(values = temp_fill_scale) + # Use custom fills
      ggplot2::scale_color_manual(values = temp_color_scale) + # Use custom colors
      ggplot2::scale_size_manual(values = temp_size_scale) + # Use custom edge size
      ggplot2::labs(x=feature_name, fill="log2 Ratio", color="log2 Ratio", size="log2 Ratio") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))

    out_plot[["ratio_by_feature"]] <-
      ggplot2::ggplot(data=df_sum) +
      ggplot2::geom_col(ggplot2::aes(x=log2_ratio_string, y=weight, fill=feature_bin, color=log2_ratio_string, size=log2_ratio_string), position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::labs(x="log2 Ratio", fill=feature_name) +
      ggplot2::scale_color_manual(values = temp_color_scale) + # Use custom colors
      ggplot2::scale_size_manual(values = temp_size_scale) + # Use custom edge size
      ggplot2::guides(color=FALSE, size=FALSE) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
  }else if (position=="fill"){

    df_sum_feature <- df_sum %>%
      group_by(feature_bin) %>%
      summarise(weight= sum(weight), .groups="drop") %>%
      mutate(prop_weight=weight/max(weight))

    out_plot[["feature_by_ratio"]] <-
      ggplot2::ggplot(data=df_sum) +
      ggplot2::geom_col(ggplot2::aes(x=feature_bin, y=weight, fill=log2_ratio_string, color=log2_ratio_string, size=log2_ratio_string), position = ggplot2::position_fill(reverse = TRUE)) +
      ggplot2::geom_line(data=df_sum_feature, ggplot2::aes(x=feature_bin, y=prop_weight, linetype="Relative feature weight", group=1), size=2) +
      ggplot2::scale_fill_manual(values = temp_fill_scale) + # Use custom fills
      ggplot2::scale_color_manual(values = temp_color_scale) + # Use custom colors
      ggplot2::scale_size_manual(values = temp_size_scale) + # Use custom edge size
      ggplot2::labs(x=feature_name, fill="log2 Ratio", color="log2 Ratio", size="log2 Ratio") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))


    df_sum_ratio <- df_sum %>%
      group_by(log2_ratio_string) %>%
      summarise(weight= sum(weight), .groups="drop") %>%
      mutate(prop_weight=weight/max(weight))

    out_plot[["ratio_by_feature"]] <-
      ggplot2::ggplot(data=df_sum) +
      ggplot2::geom_col(ggplot2::aes(x=log2_ratio_string, y=weight, fill=feature_bin, color=log2_ratio_string, size=log2_ratio_string), position = ggplot2::position_fill(reverse = TRUE)) +
      ggplot2::geom_line(data=df_sum_ratio, ggplot2::aes(x=log2_ratio_string, y=prop_weight, linetype="Relative feature weight", group=1), size=2) +
      ggplot2::scale_color_manual(values = temp_color_scale) + # Use custom colors
      ggplot2::scale_size_manual(values = temp_size_scale) + # Use custom edge size
      ggplot2::guides(color=FALSE, size=FALSE) +
      ggplot2::labs(x="log2 Ratio", fill=feature_name, y="Relative weight") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
  }



  return(out_plot)

}
