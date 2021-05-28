#' expl_na
#' @description
#' When given a dataframe object, this function will return
#' a dataframe object with 1 row per variable with the sum of NA's.
#' sorted alphabetically.
#'
#' @param df dataframe
#' @param na.strings Character - values to treat as NA
#' @param ignore.case Logical - TRUE/FALSE as to whether na.strings should be case specific
#'
#' @seealso expl_categorical
#'
#' @return
#' a dataframe containing the columns:
#' var [Character]: variables from dataframe
#' na [Integer]: count of missing values
#' perc [Numeric]: percentage of that level
#'
#' no columns: "no columns provided"
#'
#' @examples
#' df <- data.frame(pet = rep(c('dog', 'cat', 'Missing', 'Unknown'), c(40,30,20,10)),
#'                 age = rep(c(1:9,NA),10),
#'                 sex = rep(c('M','F', NA), c(50,30,20)),
#'                 stringsAsFactors = FALSE)
#' expl_na(df)
#' expl_na(df, na.strings = c('Missing'))
#' expl_na(df, na.strings = c('unknown'), ignore.case = TRUE)
#'
#' @note an example for na.strings is c("NA", "Missing", "no_xml")
#'
#' @export

expl_na <- function(df, na.strings = NULL, ignore.case = FALSE){

  # test correct inputs
  checkmate::assertDataFrame(df)
  checkmate::assert_character(na.strings,
                              null.ok = TRUE)

  checkmate::assert_logical(ignore.case)

  # get names of columns
  df.na <- data.frame(var = names(df))

  # remove all from na.strings
  if(!is.null(na.strings)){
    replace.these <- paste0('(',paste(na.strings, collapse = '|'), ')')
    df[] <- lapply(df, function(x) gsub(pattern = replace.these,
                                       replacement = NA,
                                       ignore.case =ignore.case,
                                       x = x))
  }

  # return message if no columns
  if(nrow(df.na) < 1){
    return('no columns provided')
  }else{
    df.na$na <- sapply(df, function(x) sum(is.na(x)))
  }

    # as a column percentage
  df.na$perc <- (df.na$n/nrow(df))*100

  # sort output by var then level alphabetically
  df.na <- df.na[order(df.na$var),]

  # reset row.names
  row.names(df.na) <- NULL

  return(df.na)
}
