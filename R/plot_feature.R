#' plot_feature
#'
#' @description
#' returns a plot showing the distribution of a feature also showing how the target changes across the feature
#'
#' @param feature - vector of feature values. Can be numeric, character or factor
#' @param target - vector of target. Must be numeric
#' @param n_bins - integer Number of bins to split feature into
#' @param weight - vector of weights. Default is even weights
#' @param split - optional: vector of categories, can be NB vs RN ect
#' @param feature_name - optional: used for title of plots
#' @param exposure_type - character. either \code{'pdf'} or \code{'count'}. Method used to plot exposure
#' @param use_plotly - logical. If \code{TRUE} plotly engine is used. If false ggplot is used
#'
#' @return plotly or ggplot object
#' @export
#'
#' @examples
#'
#' plot_feature(feature=seq(1,25), target=runif(25))
#' plot_feature(feature=seq(1,50), target=runif(50), split=rep(c("NB","RN"), each=25))
#' plot_feature(feature=seq(1,25), target=runif(25), exposure_type="count")
#' plot_feature(feature=rnorm(100) , target=runif(100))
#' plot_feature(feature=rnorm(100), target=runif(100), exposure_type="count")
#' plot_feature(feature=rnorm(1000), target=runif(1000), split=sample(c("a", "b", "c"), 1000, replace = TRUE, prob=c(0.5, 0.3, 0.2)))
#' plot_feature(feature=c(rnorm(900), rep(c(999, NA),50)), target=runif(1000), split=sample(c("a", "b", "c"), 1000, replace = TRUE, prob=c(0.5, 0.3, 0.2)))
#'
#' plot_feature(feature=c(rnorm(100), -999,999), target=runif(102), exposure_type="pdf")
#' plot_feature(feature=c(NA, seq(1,24)), target=runif(25), exposure_type="count")
#' plot_feature(feature=c(NA, seq(1,23), 999), target=runif(25), exposure_type="pdf")
#' plot_feature(feature=rep(c("a", "b", "c", "d", "e"), 5), target=runif(25))
#' plot_feature(feature=rep(c("a", "b", "c", "d", NA), 5), target=runif(25))
#' plot_feature(feature=rep(c("a", "b", "c", "d", NA), 5), target=runif(25), split=sample(c("a", "b", "c"), 25, replace = TRUE, prob=c(0.5, 0.3, 0.2)))
#'
plot_feature <- function(feature,
                         target,
                         n_bins=10,
                         weight=rep(1, length(feature)),
                         split=rep("feature", length(feature)),
                         feature_name="feature",
                         split_name="split",
                         exposure_type="pdf",
                         use_plotly=TRUE){

  # input validation
  checkmate::assert_numeric(target, len=length(feature), any.missing=FALSE)
  checkmate::assert_integerish(n_bins, len=1, lower=1)
  checkmate::assert_numeric(weight, len=length(feature), lower=0, any.missing=FALSE)
  checkmate::assert_vector(split, len=length(feature))
  checkmate::assert_true(split %>% unique() %>% length() < 6)
  checkmate::assert_character(feature_name, len = 1)
  checkmate::assert_character(split_name, len = 1)
  checkmate::assert_choice(exposure_type, c("count", "pdf"))
  checkmate::assert_logical(use_plotly, len=1)

  # Need to stop error due to too few colours being chosen
  if (split %>% unique() %>% length() <=2){
    colors = c("#132B43", "#56B1F7")
  }else{
    colors <- "Set1"
  }

  # plotly implementation hasn't been written
  if (use_plotly==FALSE){
    warning("ggplot implementation not written for plot_feature")
    return(ggplot2::ggplot())
  }

  if (is.numeric(feature)){ #Deal with numeric features

    #bind feature, target and weight
    df <- data.frame(feature=feature, target=target, weight=weight, split=as.factor(split))

    #Split known and unknowns
    df_known <- df %>% filter(!is.na(feature))
    df_NA <- df %>% filter(is.na(feature))

    if (nrow(df_known) > 0){ # If the dataset isn't fully NA

      # Bucket data and find outliers
      plot.data <- plotting_numerical_buckets(var_to_band=df_known$feature,
                                              n_bins=n_bins,
                                              weight=df_known$weight,
                                              include_outliers=TRUE)

      # Merge buckets with features and summaries
      find_bin <- function(x){
        ge_lower <- plot.data$lower <= x
        l_upper <- x < plot.data$upper
        e_top_upper <- plot.data$bin == max(plot.data$bin) & (x == plot.data$upper)


        plot.data$bin[which(ge_lower & (l_upper | e_top_upper))]
      }

      # Calculate value weight and target value in each bin
      df_known_sum <- df_known %>%
        mutate(bin = sapply(feature, find_bin)) %>%
        left_join(plot.data, by="bin") %>%
        group_by(bin, labels, lower, upper, center, width, split) %>%
        summarise(sd=sqrt(sum(weight * (target-(sum(target)/sum(weight)))^2)/sum(weight)),
                  se=sd/(sum(weight)^0.5),
                  weight=sum(weight),
                  target=sum(target)/sum(weight),
                  .groups = "drop")

      # Find number of unknowns and average value of the target
      df_NA_sum <- df_NA %>%
        mutate(center="<NA>") %>%
        group_by(center, split) %>%
        summarise(sd=sqrt(sum(weight * (target-(sum(target)/sum(weight)))^2)/sum(weight)),
                  se=sd/(sum(weight)^0.5),
                  weight=sum(weight),
                  target=sum(target)/sum(weight),
                  .groups = "drop")

      # Treat outliers with unknowns (which are placed on categorical axis)
      df_inlier_sum <- df_known_sum %>%
        filter(bin>0)

      df_outlier_sum <-df_NA_sum %>%
        rbind(df_known_sum %>% filter(bin<0) %>% select(center, split, sd, se, weight, target))
    }else{ # if all the data is unknown

      # Find target mean for unknowns
      df_outlier_sum <- df_NA %>%
        mutate(center="<NA>") %>%
        group_by(center, split) %>%
        summarise(sd=sqrt(sum(weight * (target-(sum(target)/sum(weight)))^2)/sum(weight)),
                  se=sd/(sum(weight)^0.5),
                  weight=sum(weight),
                  target=sum(target)/sum(weight),
                  .groups = "drop")

      # Create dummy dataset for inliers. Has no values
      df_inlier_sum <- df_outlier_sum %>%
        head(0) %>%
        mutate(width=1)

    }


    if (exposure_type=="pdf"){# If plotting probability density function calculate it
      df_inlier_sum <- df_inlier_sum %>%
        mutate(pdf=weight/width)

      df_outlier_sum <- df_outlier_sum %>%
        mutate(width=ifelse(nrow(df_inlier_sum)>0, min(df_inlier_sum$width), 1),
               pdf=weight/width)
    }else{ # If using count
      df_inlier_sum <- df_inlier_sum %>%
        mutate(pdf=weight)

      df_outlier_sum <- df_outlier_sum %>%
        mutate(pdf=weight)
    }


    if (use_plotly){


      p_inlier <- plotly::plot_ly(data=df_inlier_sum) %>%
        plotly::add_trace(y=~target, x=~center, color=~split, colors=colors, type="scatter", mode="lines+markers", legendgroup="target", xaxis="x", yaxis="y",  error_y=~list(array=se, color="#000000")) %>%
        plotly::add_trace(y=~pdf, x=~center, width=~width, color=~split, colors=colors, type="bar", legendgroup="Exposure", xaxis="x", yaxis="y2", opacity=0.2, marker=list(line=list(width=1, color='rgb(0,0,0)')))


      if (nrow(df_outlier_sum)==0){ # If there are no outliers or unknowns return inliers
        # Label and format plot
        p_inlier %>%
          plotly::layout(
            title = paste0("Distribution of ",feature_name),
            yaxis = list(title="target value"),
            yaxis2 = list(overlaying = "y", side = "right", title=glue::glue("Exposure ({exposure_type})"), showgrid = FALSE, rangemode="nonnegative"),
            barmode = "stack",
            legend = list(title=list(text=split_name))
          ) %>%
          return()
      }else{ # for outliers and unknowns

        # Turn centers into factor so axis is categorical
        df_outlier_sum <- df_outlier_sum %>%
          mutate(center=factor(df_outlier_sum$center, ordered = TRUE))

        # Find ratio of known to unknowns
        width_ratio <- pmax(1, nrow(df_inlier_sum)) / (pmax(1, nrow(df_inlier_sum)) + nrow(df_outlier_sum))

        # Plot unknowns and outliers on categorical axis
        p_all <-  p_inlier %>%
          plotly::add_trace(data=df_outlier_sum, y=~target, x=~center, color=~split, colors=colors, type="scatter", mode="lines+markers", legendgroup="target", xaxis="x2", yaxis="y", error_y=~list(array=se, color="#000000"), showlegend=(nrow(df_inlier_sum)==0)) %>%
          plotly::add_trace(data=df_outlier_sum, y=~pdf, x=~center, color=~split, colors=colors, type="bar", legendgroup="Exposure", xaxis="x2", yaxis="y2", opacity=0.2, marker=list(line=list(width=1, color='rgb(0,0,0)')), showlegend=(nrow(df_inlier_sum)==0))

        # Label and format plot
        p_all %>%
          plotly::layout(
            title = paste0("Distribution of ",feature_name),
            yaxis = list(title="target value"),
            xaxis = list(title=feature_name, domain=c(0, width_ratio - 0.025)),
            xaxis2 = list(title=feature_name, domain=c(width_ratio + 0.025, 1)),
            yaxis2 = list(overlaying = "y", side = "right", title=glue::glue("Exposure ({exposure_type})"), showgrid = FALSE, rangemode="nonnegative"),
            barmode="stack",
            legend = list(title=list(text=split_name))
          ) %>%
          return()


      }
    }



  }else{ # For character and factor features

    # summarise target over feature
    df_sum <- data.frame(feature=as.character(feature), weight=weight, target=target, split=as.factor(split), stringsAsFactors = FALSE) %>%
      mutate(feature=ifelse(is.na(feature), "<NA>", feature)) %>%
      group_by(feature, split) %>%
      dplyr::summarise(sd=sqrt(sum(weight * (target-(sum(target)/sum(weight)))^2)/sum(weight)),
                       se=sd/(sum(weight)^0.5),
                       weight=sum(weight),
                       target=sum(target)/sum(weight),
                       .groups = "drop")

    # Order feature if needed
    if (!is.ordered(feature)){
      df_sum$center <- factor(df_sum$feature,
                              ordered=TRUE)#,
      #levels=prep_char_num_sort(as.character(df_sum$feature), unique_val=TRUE))
    }else{
      df_sum$center <- df_sum$feature
    }

    #output plot
    if (use_plotly){
      plotly::plot_ly(data=df_sum) %>%
        plotly::add_trace(y=~target, x=~center, color=~split, colors=colors, type="scatter", mode="lines+markers", legendgroup="target", error_y=~list(array=se, color="#000000")) %>%
        plotly::add_trace(y=~weight, x=~center, color=~split, colors=colors, type="bar", yaxis="y2", legendgroup="Exposure", opacity=0.2, marker=list(line=list(width=1, color='rgb(0,0,0)'))) %>%
        plotly::layout(
          title = paste0("Distribution of ",feature_name),
          yaxis = list(title="target value"),
          yaxis2 = list(overlaying = "y", side = "right", title=glue::glue("Exposure ({exposure_type})"), showgrid = FALSE, rangemode="nonnegative"),
          xaxis = list(title=feature_name),
          barmode="stack",
          legend = list(title=list(text=split_name))
        ) %>%
        return()
    }

  }
}
