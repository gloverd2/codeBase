#' prep_num_bin
#'
#' @description
#' Function which bins numeric vector into bins of equal weight (or a smooth function on weight).
#'
#' @param var_to_band Vector[numeric] - vector of values to be binned. Should be true numeric however can also be categorical with fewer levels than \code{n_bins}
#' @param n_bins numeric - Number of bins to split exposure into
#' @param weight numeric - vector weight for observations
#' @param use_labels logical - should the bins be numbered (\code{FALSE}) or human readable label such as [1 - 7] (\code{TRUE})
#' @param method string - One of \code{c("even_weight", "gaussian_weight")}.
#' If \code{"even_weight"} is used the weight (exposure) in each bucket is even
#' If \code{"gaussian_weight"} is used the weight look like a normal distribution centered on \code{mean} with width \code{sd}. The effect of this is to give thinner tails
#' @param mean numeric - Only used when \code{method}=\code{"gaussian_weight"} - value between 0 - 1 for if the bucket with maximum weight is at min or max prediction. Default is 0.5 (middle bucket)
#' @param sd numeric - Only used when \code{method}=\code{"gaussian_weight"} - value between 0.1 - 1 for how thin the distribution tails should be. Low numbers give thin tails
#'
#' @return a list with the following named entries:
#' bins: Either a numeric vector or a factor (depending on \code{use_labels}) which gives the bin. The vector is of length \code{length(var_to_band)} and has \code{n_bins} unique values
#' labels: Human readable labels of where binning has occured
#' cut_values: The numbers the cuts are made at such that they can be reproduced using \code{cut} fucntion
#' vector with values 1 to \code{n_bins} indicating bin of var_to_band. (1 is low \code{var_to_band} and \code{n_bins} is high \code{var_to_band})
#' @export
#'
#' @examples
#'
#' out1 <- prep_num_bin(var_to_band=1:20, n_bins=5)
#' out2 <-prep_num_bin(var_to_band=1:20, n_bins=5, method="gaussian_weight")
#' out3 <-prep_num_bin(var_to_band=1:20, n_bins=5, method="gaussian_weight", use_labels=TRUE)
#'
#' #To reproduce using cuts
#' out1$bins
#' cut(1:20, breaks=out1$cut_values, labels=FALSE)
prep_num_bin <- function(var_to_band, n_bins=10, weight=rep(1, length(var_to_band)), method="even_weight", use_labels=FALSE, mean=0.5, sd=0.3){

  # Data prep and checking --------------------------------------------------
  checkmate::assert_choice(method, c("even_weight", "gaussian_weight"))
  checkmate::assert_integerish(n_bins, len=1, lower=2) # check n_bins is numeric of length 1
  checkmate::assert_numeric(var_to_band, min.len = n_bins) # check var_to_band is a numeric vector
  checkmate::assert_logical(use_labels, len = 1)
  checkmate::assert_numeric(weight, len=length(var_to_band), lower=0)


  # Catch edge case of being passed a categorical variable
  if (var_to_band %>% unique() %>% length() <= n_bins){

    data_labels <- var_to_band %>% unique() %>% sort()
    data_bins <- match(var_to_band, data_labels)


    if (use_labels==TRUE){
      data_bins <- factor(data_bins, levels=unique(data_bins) %>% sort(), labels = data_labels, ordered=TRUE)
    }

    return(list(bins=data_bins, labels=data_labels, cut_values=c(-Inf, data_labels %>% head(-1), Inf)))

  }

  #Note: This could all be replaced with Hmisc::wtd.quantile and cut.

  if (method == "even_weight"){
    weight_cut <- seq(0,1,length.out =  n_bins + 1)[2:n_bins] * sum(weight, na.rm = TRUE) # Find the exposure to go in each bin
  }else if (method == "gaussian_weight"){
    checkmate::assert_numeric(mean, lower=0, upper=1, len = 1)
    checkmate::assert_numeric(sd, lower=0.1, upper=1, len = 1)
    norm.shape <- dnorm(seq(0,1,length.out =  n_bins), mean=mean, sd=sd)
    cumnorm.shape <- (cumsum(norm.shape) / sum(norm.shape))[1:(n_bins-1)]
    weight_cut <- cumnorm.shape * sum(weight, na.rm = TRUE)
  }

  # Use a dataframe so columns can be ordered by one another
  data_binned <- data.frame(n=1:length(var_to_band), var_to_band=var_to_band, weight=weight) %>%
    dplyr::arrange(var_to_band) %>% # sort by var_to_band value
    dplyr::mutate(weight = ifelse(is.na(.data[["weight"]]),0,.data[["weight"]]), # remove NAs in weight
                  cumsum.weight=cumsum(weight), # cumulative exposure
                  bin=1 # place exposure into equal sized bins
    )

  # Don't place bin edge between identical values
  data_binned <- data_binned %>%
    select(-cumsum.weight) %>%
    left_join(data_binned %>%
                group_by(var_to_band) %>%
                summarise(cumsum.weight=min(cumsum.weight), .groups = "drop"),
              by="var_to_band")


  # placed back into original order
  data_binned <- data_binned %>%
    dplyr::arrange(n)

  for (ii in 1:length(weight_cut)){
    data_binned$bin = data_binned$bin + ifelse(data_binned$cumsum.weight > weight_cut[ii], 1, 0)
  }

  #If value is unknown the bin is unknown
  data_binned$bin = ifelse(is.na(data_binned$var_to_band), NA, data_binned$bin)


  # Get bins
  data_bins <- data_binned %>% dplyr::pull("bin")

  # Get bin labels
  data_labels_df <- data_binned %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(min_noft=min(var_to_band), max_noft=max(var_to_band), .groups="drop") %>%
      #min=format(min(var_to_band),scipen=5), max=format(max(var_to_band),scipen=5), .groups="drop") %>%
    dplyr::mutate(
      min_ft = format(min_noft,scipen=5, trim = TRUE),
      max_ft = format(max_noft,scipen=5, trim = TRUE),
      label=paste0("(", ifelse(!is.na(dplyr::lag(max_ft)), dplyr::lag(max_ft), min_noft), " - ", max_ft, "]")
      )

  data_labels <- data_labels_df %>% pull("label")
  cut_values <- c(-Inf, data_labels_df %>% pull("max_noft") %>% head(-1), Inf)

  if (use_labels==TRUE){
    data_bins <- factor(data_bins, levels=unique(data_bins) %>% sort(), labels = data_labels, ordered=TRUE)
  }

  return(list(bins=data_bins, labels=data_labels, cut_values=cut_values))
}
