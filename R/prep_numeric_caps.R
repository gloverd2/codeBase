#' prep_numeric_caps
#'
#' @param data vector or dataframe - data to cap. For dataframe columns are encoded. All columns must be numeric
#' @param cap_below numeric or named list of numerics. Is the lower value at which to cap
#' If a named list is given the names must be the same as the dataframe colnames
#' @param cap_above numeric or named list of numerics. Is the upper value at which to cap
#' If a named list is given the names must be the same as the dataframe colnames
#' @param unknown_below numeric or named list of numerics. Is the lower value at which to treat all below as unknown
#' If a named list is given the names must be the same as the dataframe colnames
#' @param unknown_above numeric or named list of numerics. Is the upper value at which to treat all above as unknown
#' If a named list is given the names must be the same as the dataframe colnames
#'
#' @return output the same shape as data with capping performed
#' @export
#'
#' @examples
#' prep_numeric_caps(0:10, cap_below=3, unknown_above=7)
#'
#'
#' data_in_df <- data.frame(
#'   first=seq(0,10,1),
#'   second=seq(0,100,10),
#'   third=seq(0,10,1)
#' )
#'
#' cap_below <- list(first=3, second = 30, third=NULL)
#' cap_above <- list(first=7, second = 70, third=NULL)
#' unknown_below <- list(first=1, second = 10, third=NULL)
#' unknown_above <- list(first=9, second = 90, third=NULL)
#'
#' prep_numeric_caps(data=data_in_df, cap_below=cap_below, cap_above=cap_above, unknown_below=unknown_below, unknown_above=unknown_above)
#'
prep_numeric_caps <- function(data, cap_below=NULL, cap_above=NULL, unknown_below=NULL, unknown_above=NULL){

  # If the input is a data frame call the vector version of the function multiple times
  if (class(data) %in% c("data.frame", "data.table")){
    data_df_out <- data # Create a dataframe of the correct shape

    # Loop over all columns in the dataset
    for (ii in 1:ncol(data)){

      col_ii <- colnames(data)[ii]

      # get the values for each of the inputs for the column
      # If the input is a list use the named value (from column name)
      # If the input isn't a list use the same value for all
      for (var_name in c("cap_below", "cap_above", "unknown_below", "unknown_above")){
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
      data_df_out[[col_ii]] <- prep_numeric_caps(data[[col_ii]], cap_below=cap_below_ii, cap_above=cap_above_ii, unknown_below=unknown_below_ii, unknown_above=unknown_above_ii)

    }

    return(data_df_out)

  }else{

  # check types
  checkmate::assert_numeric(data)
  checkmate::assert_numeric(cap_below, null.ok=TRUE)
  checkmate::assert_numeric(cap_above, null.ok=TRUE)
  checkmate::assert_numeric(unknown_below, null.ok=TRUE)
  checkmate::assert_numeric(unknown_above, null.ok=TRUE)

  # Check ordering
  if(!is.null(cap_below) & !is.null(cap_above)){
    checkmate::assert_true(cap_below < cap_above)
  }
  if(!is.null(unknown_below) & !is.null(unknown_above)){
    checkmate::assert_true(unknown_below < unknown_above)
  }
  if(!is.null(cap_below) & !is.null(unknown_below)){
    checkmate::assert_true(unknown_below < cap_below)
  }
  if(!is.null(cap_above) & !is.null(unknown_above)){
    checkmate::assert_true(unknown_above > cap_above)
  }

  # Cap data
  data_out <- data
  if(!is.null(cap_below)){data_out[data < cap_below] <- cap_below}
  if(!is.null(cap_above)){data_out[data > cap_above] <- cap_above}
  if(!is.null(unknown_below)){data_out[data < unknown_below] <- NA} # order is important. NA trumps cap
  if(!is.null(unknown_above)){data_out[data > unknown_above] <- NA} # order is important. NA trumps cap

  return(data_out)
  }

}


