#' check_all_identical
#' @description
#' This is a basic function which takes a list and checks that every value within the list is identical.
#' It is simply a recursive function as any failure would result in a FALSE.
#' Note: Identicality is a stronger condition than Equality. E.g 1L == 1 but identicaL(1L,1) is FALSE.
#'
#' @param input List - This is the list of values to perform an identical check on
#'
#' @return Boolean - TRUE if all are identical. FALSE if they are not all identical.
#'
#' @section Functions dependent on this: prep_char_num_sort
#'
#' @examples
#' check_all_identical(as.list(c(NA,NA,NA)))
#' check_all_identical(list(1,1L,1.00001))
#' check_all_identical(as.list(c("A","A","A","B")))
#' check_all_identical(list(as.double(NA),as.logical(NA)))
#' check_all_identical(list(1,1,1,1,1L))
#' @export

check_all_identical <- function(input){

  # Need to enforce a list. Lists can handle multiple types whereas arrays will force the same. Using c(2,"2") will convert both to numeric in an array but NOT in a list.
  checkmate::assert(class(input) == "list", .var.name = "Input MUST be a list. This is not an array. To convert an array to list use as.list()")
  # When only one thing remaining, no comparison to be made.
  if (length(input) == 1) {
    return(TRUE)
  } else {
    # Check that list object 1 and list object 2 are identical. Use an and condition with that an identical check of all objects except 1 (through recursion)
    # The full evaluation should be checking (1,2), (2,3), (3,4) etc .... If any of these fail, then the combined and conditions should fail.
    return(identical(input[1],input[2]) & check_all_identical(input[-1]))
  }
}

