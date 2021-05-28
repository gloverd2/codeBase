#' encode_freq
#'
#' @description
#' This function takes one or more categorical variables and tranforms transforms them using frequency encoding.
#' The first level (1) is Unknown values, the second level (2) is the most popular level, third (3) the second most popular level.
#' Small levels can be grouped together using the n_level flag. These are placed at the end
#'
#' @param data vector or dataframe - data to frequency encode. For dataframe columns are encoded
#' @param n_levels numeric or named list of numerics - This is the maximun number of categorical levels to include (note "Unknown" and "Other" are always added).
#' If a named list is given the names must be the same as the dataframe colnames
#' @param min_level_count numeric or named list of numerics - This in the minimum number of instances for a level to be counted (This is stricter than a given value for n_levels)
#' If a named list is given the names must be the same as the dataframe colnames
#' @param unknown_levels vector[String] or named list of vector[Sting] - These values will be treated as unknown. NA and "" are always treated as unknown
#' If a named list is given the names must be the same as the dataframe colnames
#' @param unknown_treatment_method numeric or named list of numerics - Must be 1. Gives option to treat unknowns differently. Not implememted
#' If a named list is given the names must be the same as the dataframe colnames
#'
#' @return list(data, levels) -
#' data is transformed data in the same shape as the input \code{data}
#' levels is a vector (if \code{data} is vector) or named list of vectors (if \code{data} is dataframe) containing the order of the categorical levels
#' @export
#'
#' @examples
#'
#' data_in <- c(rep("cat", 2) , rep("dog", 3), rep("fish", 4), "llama", NA)
#' encode_freq(data=data_in)
#'
#' data_in_df <- data.frame(
#' pet=c(rep("cat", 2) , rep("dog", 3), rep("fish", 4), "llama", NA),
#' letter=c(rep("a",5), rep("b",5), "c")
#' )
#'
#' encode_freq(data=data_in_df)
#'
encode_freq <- function(data, n_levels=NULL, min_level_count=NULL, unknown_levels=NULL, unknown_treatment_method=1){

  # If the input is a data frame call the vector version of the function multiple times
  if (class(data) %in% c("data.frame", "data.table")){
    data_df_out <- data # Create a dataframe of the correct shape
    levels_out <- list() # Create empty list to contain levels

    # Loop over all columns in the dataset
    for (ii in 1:ncol(data)){

      col_ii <- colnames(data)[ii]

      # get the values for each of the inputs for the column
      # If the input is a list use the named value (from column name)
      # If the input isn't a list use the same value for all
      for (var_name in c("n_levels", "min_level_count", "unknown_levels", "unknown_treatment_method")){
        if (!is.null(get(var_name))){
          if (length(get(var_name))>1){
            checkmate::assert_names(names(get(var_name)), must.include=col_ii)
            assign(paste0(var_name,"_ii"), get(var_name)[[col_ii]])
          }else{
            assign(paste0(var_name,"_ii"), get(var_name))
          }
        }else{
          assign(paste0(var_name,"_ii"), NULL)
        }
      }

      # Call the vector version of the function
      data_ii <- encode_freq(data[[col_ii]], n_levels=n_levels_ii, min_level_count=min_level_count_ii , unknown_levels=unknown_levels_ii, unknown_treatment_method=unknown_treatment_method_ii)

      # Use output to populate data and levels list
      data_df_out[[col_ii]] <- data_ii$data
      levels_out[[col_ii]] <- data_ii$levels
    }

    return(list(data=data_df_out, levels=levels_out))

  }else{

    # To change when other unknown treatment methods are added
    checkmate::assert_choice(unknown_treatment_method, c(1))
    checkmate::assert_number(n_levels, null.ok=TRUE)

    data_c <- as.character(data)
    data_c[data_c %in% c(unknown_levels, "") | data %in% unknown_levels ] <- NA

    sorted_names_values <- data_c %>% table(useNA="no") %>% sort(decreasing = TRUE)
    sorted_names_t <- sorted_names_values %>% names()

    # If there is a min number of counts for levels to be counted let this be a stricter criteria than n_levels
    if(!is.null(min_level_count)){
      n_levels <- min(sum(sorted_names_values >= min_level_count), n_levels)
    }

    # Remove null value
    if(is.null(n_levels)){n_levels <- length(sorted_names_t)}

    if (unknown_treatment_method==1){
      if (length(sorted_names_t) <= n_levels ){ # If the number of levels doesn't need capping
        sorted_names <- unique(c("Unknown", sorted_names_t, "Other"))
      }else{ # If the number of levels does need capping
        sorted_names <- unique(c("Unknown", sorted_names_t[1:n_levels], "Other"))
      }
    }

    data_c[is.na(data_c)] <- "Unknown" # NA -> "Unknown"
    data_c[!(data_c %in% sorted_names)] <- "Other" # Other -> "Other"

    #Create ordered factor
    data_out <- factor(data_c, ordered=TRUE, levels=sorted_names)
    # Order factor to numeric
    data_out_n <- as.numeric(data_out)

    return(list(data=data_out_n, levels=sorted_names))
  }
}

