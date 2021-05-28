  #' expl_categorical
  #' @description
  #' When given a dataframe object, this function will return
  #' a dataframe object with 1 row per level for variable
  #' sorted alphabetically.
  #'
  #' It will provide:
  #' how many rows contain that level
  #' and the percentage of that value for that variable.
  #'
  #' @param df dataframe - train set.
  #' @param char.level Numeric - exclude character|factor columns if levels > char.level
  #' @param num.level Numeric - exclude numeric|integer columns if levels > num.level (NULL discards numeric columns)
  #'
  #' @return
  #' a dataframe containing the columns:
  #' var [Character]: a character variable
  #' level [Character]: a level for that character variable
  #' class [Character]: what class is the variable
  #' n [Integer]: count of that level
  #' perc [Numeric]: percentage of that level
  #'
  #' no character columns: "no character or factors"
  #' too many levels in all character columns: "no columns with fewer levels than char.level"
  #'
  #' @examples
  #' df <- data.frame(pet = rep(c('dog', 'cat', 'horse', 'hamster'), c(40,30,20,10)),
  #'                 age = 1:100,
  #'                 size = factor(rep(c('Small','Large', NA), c(50,30,20)), levels = c('Small', 'Large'), ordered = TRUE),
  #'                 stringsAsFactors = FALSE)
  #' expl_categorical(df, char.level = 3)
  #' expl_categorical(df, char.level = 5)
  #'
  #' @export

  expl_categorical <- function(df, char.level = 20, num.level = 20){

    # test correct inputs
    checkmate::assertDataFrame(df)
    checkmate::assert_int(char.level,
                         lower = 0,
                         upper = 100,
                         null.ok = FALSE)

    checkmate::assert_int(num.level,
                          lower = 0,
                          upper = 100,
                          null.ok = FALSE)

    # get the classes for all variables in df
    class_info <- df %>%
                  dplyr::summarise_all(. ,function(x){class(x)[1]}) %>%
                  base::t() %>%
                  base::as.data.frame() %>%
                  dplyr::mutate(var = row.names(.)) %>%
                  dplyr::rename('class' = V1)

    # get number of distinct values per variable
    # drop any character/factors that have more levels than char.level
    # drop any numerics/integers that have more levels than num.level
    var_info <- df %>%
                dplyr::summarise_all(dplyr::n_distinct) %>%
                base::t() %>%
                base::as.data.frame(stringsAsFactors=FALSE) %>%
                dplyr::rename('n' = V1) %>%
                base::cbind(class_info) %>%
                dplyr::filter(!(class %in% c('integer','numeric') & n > num.level | class %in% c('character', 'ordered', 'factor') & n > char.level)) %>%
                dplyr::select(var)

    rownames(var_info) <- NULL

    # if there are no variables remaining
    if(length(var_info$var)<1){
      warning('no columns with fewer levels than either char.level or num.level')
      return(data.frame(NULL))}

    # create a dataframe with 1 line per var, level pair
    # join class info and create a percentage for that level
    # rearrange columns for output using select as relocate with dplyr v1
    ret <- df  %>%
           dplyr::select(var_info$var) %>%
           dplyr::mutate_if(is.factor, as.character) %>%
           tidyr::gather(key = "var", value = "level") %>%
           dplyr::add_count(var, level) %>%
           dplyr::distinct() %>%
           dplyr::left_join(class_info, by = 'var') %>%
           dplyr::mutate(perc = 100*n/nrow(df)) %>%
           dplyr::mutate(n = as.integer(n)) %>%
           dplyr::arrange(var, level) %>%
           dplyr::select(var, level, class, n, perc) %>%
           dplyr::mutate_all(unname) %>%
           dplyr::mutate_if(is.factor, as.character) %>%
           base::as.data.frame(stringsAsFactors=FALSE)

    return(ret)
  }
