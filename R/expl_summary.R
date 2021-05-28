
#' expl_summary
#' @description
#' Will give the mean, standard deviation, length, and NA count for the summarized variable.
#' Variables can be added to create groupings.
#'
#' @param .data Dataframe or data table
#' @param summarise_vars Vector of numeric or integer column name strings to summarize.
#' @param group_vars Vector of column name strings to group by.
#' @param remove_missing Boolean for na.rm in mean and standard deviation.
#' @param row_limit Integer row limit.
#' @param order_col_by_stat Boolean to sort output columns sorted by statistic. Useful for comparing similar variables in summarise_vars.
#'
#' @return
#' a dataframe containing the columns:
#' One column per grouped variable: a character variable,
#' mean [Numeric]: The mean of the summarized variable,
#' sd [Numeric]: The standard deviation of the summarized variable,
#' n [Integer]: The count of the summarized variable,
#' na_count [Integer]: The count of NAs in the summarized variable.
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(score_raw = rnorm(mean = 10, n = 100),
#'                  dataset = factor(rep(c("VAL", "TRAIN", "TEST"), c(20, 70, 10)),
#'                                   levels = c("TRAIN", "VAL", "TEST")),
#'                  target = rep(c(1, 0), c(80, 20)),
#'                  stringsAsFactors = FALSE)
#'
#' df$score10 <- cut(df$score_raw,
#'                   breaks = quantile(df$score_raw, seq(0, 1, 0.1)),
#'                   labels = 1:10,
#'                   include.lowest = TRUE)
#'
#' df %>% expl_summary(summarise_vars = "score_raw")
#' df %>% expl_summary(summarise_vars = "score_raw", group_vars = "dataset")
#' df %>% expl_summary(summarise_vars = c("score_raw", "target"), group_vars = c("dataset", "score10"))
#'
#' @export

expl_summary <- function(.data, summarise_vars = NULL, group_vars = NULL, remove_missing = TRUE, row_limit = 10000, order_col_by_stat = FALSE) {

  checkmate::assert(any(class(.data) %in% c("data.table", "data.frame")), .var.name = ".data must be a data.frame or data.table")
  
  is_data_table <- FALSE
  # Only executes if data type is data.frame
  if(data.table::is.data.table(.data)){
    .data <- as.data.frame(.data)
    is_data_table <- TRUE 
    # Using this as output
  }
  
  # Assertions on other options
  checkmate::assertFlag(remove_missing, na.ok = FALSE, null.ok = FALSE, .var.name = "remove_missing must be logical")
  checkmate::assertFlag(order_col_by_stat, na.ok = FALSE, null.ok = FALSE, .var.name = "order_col_by_stat must be logical")
  
  # Checking summarise_vars and group_vars exist correctly
  # Existence check  
  checkmate::assert(!is.null(summarise_vars) & all(summarise_vars %in% colnames(.data)), .var.name = "summarise_vars must be in .data")
  checkmate::assert(is.null(group_vars) | all(group_vars %in% colnames(.data)), .var.name = "group_vars must be in .data")
  # Class check
  checkmate::assert(is.null(summarise_vars) | all(sapply(.data[,summarise_vars], class) %in% c("numeric", "integer")), .var.name = "summarise_vars must be numeric or integer columns")
  
  # Check row_limit works correctly
  checkmate::assertIntegerish(row_limit, lower = 0, null.ok = FALSE, len = 1, .var.name = "row_limit must be numeric")
  if (nrow(unique(.data[group_vars])) > row_limit) {
    warning("Exceeded row limit. Reduce number of variables in 'group_vars' and/or increase 'row_limit'.")
  }
  checkmate::assert(nrow(unique(.data[group_vars])) <= row_limit, .var.name = "output within row_limit")
  
  # Creating a way to order the columns
  col_order <- group_vars
  if ((!order_col_by_stat) & (length(summarise_vars) > 1)){
    for(var in summarise_vars){
      col_order <- c(col_order, paste0(var,c("_mean","_sd","_n","_na_count")))
    }
  } else if ((length(summarise_vars) > 1)){
    for(suffix in c("_mean","_sd","_n","_na_count")){
      col_order <- c(col_order, paste0(summarise_vars, suffix))
    }
  } else {
    for(var in summarise_vars){
      col_order <- c(col_order, c("mean","sd","n","na_count"))
    } 
  }

  # Aggregation
  output <- .data %>%
    dplyr::group_by_at(dplyr::vars({{group_vars}})) %>%
    dplyr::summarise_at(.vars = dplyr::vars({{summarise_vars}}),
                        .funs = list(mean = ~ mean(., na.rm = remove_missing),
                                     sd = ~ sd(., na.rm = remove_missing),
                                     n = length,
                                     na_count = ~ sum(is.na(.)))) %>%
    ungroup() %>%
    dplyr::select(all_of(col_order))
  if (is_data_table) {
    return(output %>% data.table::as.data.table())
  } else {
    return(output %>% as.data.frame())
  }
}

