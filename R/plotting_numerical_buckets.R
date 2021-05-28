#' plotting_numerical_buckets
#'
#' @description
#' This function takes a numeric vector \code{var_to_band} and groups it into buckets which are useful for plotting.
#' These groups have approximately equal weight
#' Outliers are excluded/included using the \code{include_outliers} option.
#' This function is built on \code{\link{prep_num_bin}}
#'
#' @param var_to_band Vector[numeric] - vector of values to be binned
#' @param n_bins numeric - Number of bins to split exposure into
#' @param weight numeric - vector weight for observations
#' @param include_outliers logical - should outliers be trimmed
#'
#' @return data.frame with columns
#' bin [numeric]: numeric label of bin
#' labels [character]: string output from \code{\link{prep_num_bin}} if called
#' lower [numeric]: Min bound of the bin
#' upper [numeric]: Max bound of the bin
#' center [numeric] : Center of the bin - (upper + lower) / 2
#' width [numeric] : Width of the bin - (upper - lower)
#' @export
#'
#' @examples
#' plotting_numerical_buckets(var_to_band=runif(100), n_bins=10)
plotting_numerical_buckets <- function(var_to_band, n_bins=25, weight=rep(1, length(var_to_band)), include_outliers=FALSE){

  checkmate::assert_numeric(var_to_band)
  checkmate::assert_integerish(n_bins, len=1)
  checkmate::assert_logical(include_outliers, len=1)
  checkmate::assert_numeric(weight, len=length(var_to_band), lower=0)


  if(var_to_band %>% unique() %>% length() < pmax(10, (n_bins*2))){ # low number of levels. Just return the number of levels

    binning_df <- data.frame(bin=1:(var_to_band %>% round(digits=10) %>% unique() %>% length()),
                             labels="",
                             center=var_to_band %>% round(digits=10) %>% unique() %>% sort(),
                             stringsAsFactors = FALSE) %>%
      dplyr::mutate(width=min(c(1, center - dplyr::lag(center)), na.rm=TRUE),
                    lower=center - width/2,
                    upper=center + width/2)

    return(binning_df)

  }else{ # Data needs splitting into buckets


    # Remove outliers. Looking for very odd values,such as 9999 when the rest of the data is in the range 0-100
    # This function works recursively by hypothesizing the largest or lowest value is an outlier
    # The function and is willing to accept up to 5 unique high and low outliers. - If it detects more it gives up.
    # Outliers are defined as being far way from the mean of the distribution when the outlier(s) are removed, far is defined as 7.5 standard deviations (the standard deviations is calculated when the outlier(s) are removed)
    # Outliers are also defined as the only negative value (although this part isn't recursive)
    # This function tries to return the min and max subvar_to_band without outlisers.
    find_limits <- function(subvar_to_band, run=0){

      min_value <- min(subvar_to_band)
      max_value <- max(subvar_to_band)
      the_rest <- subvar_to_band[subvar_to_band!=min_value & subvar_to_band!=max_value]

      if (run>=5){return(c(NA, NA))}

      if (min_value<0 & min(the_rest)>=0 & run==0){
        lower.start=min(the_rest)
      }else if (min_value < (mean(the_rest) - 7.5 * sd(the_rest))){
        lower.start <- find_limits(the_rest, run=run+1)[1]
      }else{
        lower.start=min_value
      }


      if (max_value > (mean(the_rest) + 7.5 * sd(the_rest))){
        upper.start <- find_limits(the_rest, run=run+1)[2]
      }else{
        upper.start=max_value
      }

      return(c(lower.start, upper.start))
    }

    find_limits.out <- find_limits(var_to_band)
    lower.start <- coalesce(find_limits.out[1], min(var_to_band))
    upper.start <- coalesce(find_limits.out[2], max(var_to_band))


    # Split data into outliers in inliers

    data_outliers <- var_to_band[var_to_band<lower.start | var_to_band>upper.start]
    outlier_values <- data_outliers %>% sort() %>% unique()

    if (length(outlier_values) < 5){ # Are only really outliers if there aren't many unique values
      data_inliers <- var_to_band[var_to_band>=lower.start & var_to_band<=upper.start]
      weight_inliers <- weight[var_to_band>=lower.start & var_to_band<=upper.start]
    }else{ # if outliers aren't real use full data as inliers
      data_inliers <- var_to_band
      weight_inliers <- weight

      # remove outliers
      data_outliers <- numeric(0)
      outlier_values <- numeric(0)
    }

    #Bin inliers
    binning_out <- prep_num_bin(var_to_band=data_inliers, n_bins=n_bins, weight=weight_inliers)

    binning_df <- data.frame(data=data_inliers, bin=binning_out$bins) %>%
      group_by(bin) %>%
      summarise(lower=min(data), .groups="drop") %>%
      mutate(upper=coalesce(lead(lower), max(data_inliers)),
             labels=binning_out$labels,
             center=(upper+lower)/2,
             width=upper-lower)

    # Add outliers
    if (include_outliers==TRUE & length(outlier_values)>0){

      width_t = min(c(1, outlier_values-lag(outlier_values), binning_df$width), na.rm=TRUE)

      outliers_df <- data.frame(bin=-(1:length(outlier_values)), center=outlier_values, labels="") %>%
        dplyr::mutate(lower=center-(width_t/2), upper=center+(width_t/2), width=width_t)

      binning_df <- rbind(binning_df, outliers_df)

    }

    return(binning_df)
  }

}
