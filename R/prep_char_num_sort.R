#' prep_char_num_sort
#' @description
#' This is a function which can be used to sort arrays of numeric and characters.
#' This allows you to get numbers into order as characters. e.g. (1,2,100) rather than alphabetical (1,100,2).
#' This can sometimes when creating other functions which require you to have character inputs, as these quite often will cause values to be out of place.
#' NOTE: This function uses regex to determine what counts as a string, if some columns have symbols, they may fail to be read.
#'
#' @section Inputs:
#' @param array Array[Character] - This is the array which we want to sort. (If you create c(1,2,"3") they all get assigned character.)
#' @param NA_val Character/Numeric/NA (Default = "NA") - This is the value which we want to put NAs as.
#'   I recommend using "_NA" when you need to treat NAs as a character to get it positioned near the start rather than in the middle which "NA" would do.
#'   If NA is not NA and cannot be converted to numeric, NA will get treated as character and therefore get affected by char_decreasing.
#' @param char_decreasing Boolean/NULL (Default = FALSE) - Set this to TRUE if you want character values sorted into descending order. Set to NULL if you do not want character values sorted.
#' @param num_decreasing Boolean/NULL (Default = FALSE) - Set this to TRUE if you want numeric values sorted into descending order. Set to NULL if you do not want numeric values sorted.
#' @param unique Boolean (Default = TRUE) - Set this to true if you want unique sorted outputs after handling NAs.
#'
#' @return Array[Character] - This will be the entries in array_to_sort sorted. Sections sorted are dependent on char_decreasing and num_decreasing.
#' This could be unique dependent on the parameter "unique".
#' Note that all of these will be character values.
#'
#' @section Functions dependant on this:
#' expl_band_cond_shift
#'
#' @section Functions needed in this:
#' check_all_identical
#'
#' @examples
#' prep_char_num_sort(c("100","C","M","T","-55555","-44444","-33333","0",NA))
#' # Output: c("_NA", "C", "M", "T", "-55555", "-44444", "-33333", "0" , "100")
#' prep_char_num_sort(c("100","C","M","T","-55555","-44444","-33333","0",NA),NA_val = "-1")
#' # Output: c("C", "M", "T", "-55555", "-44444", "-33333", "-1", "0" , "100")
#'
#' @export

prep_char_num_sort <- function(array, NA_val="_NA", char_decreasing=FALSE, num_decreasing=FALSE, unique_val=FALSE){

  # Originally, we intended for this function to sort purely numerics and characters apart, but we thought of the case of wanting something that could sort version numbers.
  # i.e. "document_3_update_1", "document_2_update_2".
  # This implementation will address the original case but is mainly built in a way to deal with values such as these.
  # We might also have terms for an interaction such as C_1, C_10, C_2 ... which would not sort correctly previously.
  # The way this is implemented is a Divide and Conquer Algorithm.

  # This function might be a bit complicated to understand overall (particularly if you haven't done Object Oriented Programming), so this is a summary:
  # The idea of this function is that we extract the first part of the name, then filter by each of these groups separately and sort by the remainder of their name.
  # Eventually this should sort into every unique combination as we keep recursing deeper into the function.
  # So for example, imagine we have "file_1","file_2_subfolder_3","file_2_subfolder_1","file_2".
  # E.g. all these have "file_", so we would run the function again on "1","2_subfolder_3","2_subfolder_1","2". These would be defined in "remainder" (although "remainder" depends on the first_term chosen).
  # Now, there is a split here. We need to run the function again on just those that start with 1 and then do a separate run on those that start with 2.
  # For the case with just "1", there is only one entry - so this is already sorted. This needs to terminate early in our code, via the check_all_identical function.
  # With "2" we need to sort these, but we still need more information on how to sort those with _subfolder, so we run the sort again on these on levels without the "2".
  # At this point, "1" is evaluated so this does not recurse further.
  # As the function calls complete as far as they can go, we need the code after to recombine all of the components that we have taken out. This is what final_vector stores as new_array iterates results out.

  # Basic assertions
  checkmate::assert_vector(array, strict = TRUE, min.len = 1)
  checkmate::assert_logical(char_decreasing, null.ok = TRUE, len=1)
  checkmate::assert_logical(num_decreasing, null.ok = TRUE, len=1)
  checkmate::assert_logical(unique_val, null.ok = FALSE, len=1)
  checkmate::assert(!identical(NA_val,TRUE))  # Need identically here because TRUE == 1
  checkmate::assert(!identical(NA_val,FALSE))
  checkmate::assert(length(NA_val) == 1)

  # Ordered Factors ---------------------------------------------------------

  # If Ordinal factor the sorting is simple
  if (is.ordered(array)){

    #Replace NAs if needed
    if (any(is.na(array))){
      array_out <- factor(array, levels = levels(addNA(array)), labels = c(levels(array), NA_val), exclude = NULL, ordered = TRUE)
    }else{
      array_out <- array
    }

    array_out <- sort(array_out, decreasing=coalesce(char_decreasing, num_decreasing, FALSE))
    if (unique_val==TRUE){
      return(unique(array_out))
    }else{
      return(array_out)
    }
  }


  # Replace NAs and convert to string
  array_na <- array %>% tidyr::replace_na(NA_val)
  if (unique_val){
    array_na <- array_na %>% unique()
  }



  #Split all strings into 2 parts. Leading number and trailing characters
  #For only numeric values to trailing characters are blank
  #For values starting with characters the trailing numbers are blank

  start_num <-  array_na %>%  grep("^-?\\d+\\.?\\d*",., value =TRUE)
  start_char <- array_na[!(array_na %in% start_num)]


  array_out <- c()
  # Sort parts starting with a string
  # Find string until a number if there is one
  if (length(start_char) > 0){
    # This regex includes a backreference. \\1 returns match in first braket. This is the character before first number if there is one.
    # See
    # https://stringr.tidyverse.org/articles/regular-expressions.html
    # for more detail
    start_char_char_part <- start_char %>% gsub("(^\\D+)(-?\\d+\\.?\\d*)(\\D*)(.*)", "\\1", .) %>% unique()
    if (!is.null(char_decreasing)){
      start_char_char_part <- start_char_char_part %>% sort(decreasing=char_decreasing)
    }

    for (cc in start_char_char_part){

      #Replace regex functions
      cc <- gsub(".", "\\.", cc, fixed = TRUE)
      cc <- gsub("$", "\\$", cc, fixed = TRUE)
      cc <- gsub("?", "\\?", cc, fixed = TRUE)

      if (ifelse(is.null(char_decreasing), TRUE, char_decreasing==FALSE)){
        array_out <- c(array_out, grep(glue::glue("^{cc}$"), array_na, value = TRUE))
      }

      # If the test is followed by a string
      leftovers <- grep(glue::glue("^{cc}\\d+.*"), array_na, value = TRUE)

      if (length(leftovers)>0 & !(stringr::str_sub(cc,-1) %in% c("-","."))){
        leftovers_suffix <- leftovers %>% gsub(glue::glue("(^{cc})(\\d+.*)"), "\\2", .)
        leftovers_suffix_sorted <- prep_char_num_sort(leftovers_suffix, char_decreasing=char_decreasing, num_decreasing=num_decreasing)
        array_out <- c(array_out, paste0(cc,leftovers_suffix_sorted))
      }

      if (ifelse(is.null(char_decreasing), FALSE, char_decreasing==TRUE)){
        array_out <- c(array_out, grep(glue::glue("^{cc}$"), array_na, value = TRUE))
      }

    }
  }
  if (length(start_num) > 0){
    start_num_num_part <- start_num %>% gsub("^(-?\\d+\\.?\\d*)(\\D*)(.*)", "\\1", .)

    df_sorting <- data.frame(char=start_num_num_part, num=as.numeric(start_num_num_part), stringsAsFactors = FALSE)

    # Sort numbers also have sort char to distinquish "01" vs "1"
    if (is.null(num_decreasing)){
      #pass
    }else if (num_decreasing==FALSE){
      df_sorting <- df_sorting %>% arrange(num, char)
    }else{
      df_sorting <- df_sorting %>% arrange(desc(num), char)
    }
    start_num_num_part_unique_sorted <- df_sorting$char %>% unique()

    #Loop over all unique
    for (nn in start_num_num_part_unique_sorted){

      if (ifelse(is.null(char_decreasing), TRUE, char_decreasing==FALSE)){
        array_out <- c(array_out, grep(glue::glue("^{nn}$"), array_na, value = TRUE))
      }

      leftovers <- grep(glue::glue("^{nn}\\D+.*"), array_na, value = TRUE)
      if (length(leftovers)>0){
        leftovers_suffix <- leftovers %>% gsub(glue::glue("(^{nn})(\\D+.*)"), "\\2", .)

        leftovers_suffix_sorted <- prep_char_num_sort(leftovers_suffix, char_decreasing=char_decreasing, num_decreasing=num_decreasing)
        array_out <- c(array_out, paste0(nn,leftovers_suffix_sorted))
      }

      if (ifelse(is.null(char_decreasing), FALSE, char_decreasing==TRUE)){
        array_out <- c(array_out, grep(glue::glue("^{nn}$"), array_na, value = TRUE))
      }

    }
  }

  # Place NAs at the start
  if (is.na(NA_val)){
    array_out <- c(rep(NA, sum(is.na(array))), array_out)
  }

  checkmate::assert_true(length(array_out)==length(array_na))
  checkmate::assert_true(setequal(array_out,array_na))

  # if (any(is.na(suppressWarnings(as.numeric(array_out))))){
  #   return(array_out)
  # }else{
  #   return(array_out %>% as.numeric())
  # }

  return(array_out)
}
