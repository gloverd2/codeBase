testthat::context("Testing prep_char_num_sort")


testthat::test_that("Testing properties",{

  # One thing to note is that numbers in arrays with strings, get converted immediately to strings.
  testthat::expect_equal(c(1,"A",2),c("1","A","2"))


  # Check that it does sort multidigit numbers. If this did not work, we would see 1,100,2,50 by taking alphabetical with digit size.
  testthat::expect_equal(prep_char_num_sort(c(1,100,2,50,NA),NA_val = NA),c(NA,"1","2","50","100"))
  testthat::expect_equal(prep_char_num_sort(c("G",100,"A",1,2,50,NA),NA_val = NA),c(NA, "A","G","1","2","50","100"))


  # Check that it works with only numeric values and returns a numeric output (if NA_val is not a string).
  testthat::expect_equal(prep_char_num_sort(c(1,100,2,50,NA),NA_val = NA),c(NA,"1","2","50","100"))
  testthat::expect_equal(prep_char_num_sort(c(1,100,2,50,NA),NA_val = 2), c("1","2","2","50","100"))
  testthat::expect_equal(prep_char_num_sort(c(1,100,2,50,NA),NA_val = "A"),c("A","1","2","50","100"))

  # NA_val being character with no NAs, should be numeric.
  testthat::expect_equal(prep_char_num_sort(c(1,100,2,50),NA_val = "A"),c("1","2","50","100"))


  # NA_val is character.
  testthat::expect_equal(prep_char_num_sort(c("A","B","1",NA,NA,"3","C","2"),NA_val = "B"),c("A","B","B","B","C","1","2","3"))
  # NA_val is numeric.
  testthat::expect_equal(prep_char_num_sort(c("A","B","1",NA,NA,"3","C","2"),NA_val = 2),c("A","B","C","1","2","2","2","3"))
  # NA_val is NA.
  testthat::expect_equal(prep_char_num_sort(c("A","B","1",NA,NA,"3","C","2"),NA_val = NA),c(NA,NA,"A","B","C","1","2","3"))




  # unique TRUE
  testthat::expect_equal(prep_char_num_sort(c("B","B","A","A","3","3","1","2"),unique_val = TRUE),c("A","B","1","2","3"))
  # unique_val FALSE
  testthat::expect_equal(prep_char_num_sort(c("B","B","A","A","3","3","1","2"),unique_val = FALSE),c("A","A","B","B","1","2","3","3"))


  # Test on NAs.
  testthat::expect_identical(prep_char_num_sort(c(NA,NA,NA),unique_val = TRUE),"_NA")

  # Negative numbers.
  testthat::expect_equal(prep_char_num_sort(c("C","M","T","-55555","-44444","-33333","0",NA),NA_val = "-1"),c("C","M","T","-55555","-44444","-33333","-1","0"))
  testthat::expect_equal(prep_char_num_sort(c("C","M","T","-55555","-44444","-33333","0",NA),NA_val = -1),c("C","M","T","-55555","-44444","-33333","-1","0"))


  # Strange inputs
  testthat::expect_equal(prep_char_num_sort(1),"1")
  testthat::expect_equal(prep_char_num_sort("A"),"A")
  testthat::expect_equal(prep_char_num_sort(NA),"_NA")



  testthat::expect_equal(prep_char_num_sort(c("A_1","A_999","A_3","A_10","A_2","B_1","B_999","B_50")),c("A_1","A_2","A_3","A_10","A_999","B_1","B_50","B_999"))

  testthat::expect_equal(prep_char_num_sort(c("A_1","A_01","A_2")),c("A_01","A_1","A_2"))
  testthat::expect_equal(prep_char_num_sort(c("A_1_B","A_1_A","A_2_A")),c("A_1_A","A_1_B","A_2_A"))
  testthat::expect_equal(prep_char_num_sort(c("A_1_B_02","A_1_B_1", "A_1_C_1")),c("A_1_B_1", "A_1_B_02", "A_1_C_1"))
  testthat::expect_equal(prep_char_num_sort(c("A_1_A_1_A","A_1_A_2_A", "A_1_A_1_B")),c("A_1_A_1_A", "A_1_A_1_B", "A_1_A_2_A"))

  # We are using regex to determine what counts as a chunk, so we need to test various common symbols to see if these get carried through
  testthat::skip("sorting different in devtools")

  # Not affected by casing.
  # i.e. We do not find that it is sorted by ASCII byte value, as lower case are after all upper case.
  # The most important thing is that words do not separate due to case.
  # If this was what happened, we would find the order Aa Ab aA.
  testthat::expect_equal(prep_char_num_sort(c("Ab","aA","Aa")),c("aA","Aa","Ab"))

  testthat::expect_equal(prep_char_num_sort(c("_",".","`","''","@","%","$","£","!","~","#"," ","&","?")),c(" ", "_", "!", "?", ".", "''", "@", "&", "#", "%", "`", "~", "$", "£"))

  # Input with character, numeric and NA
  testthat::expect_equal(prep_char_num_sort(c("A","B","1",NA,NA,"3","C","2")),c("_NA","_NA","A","B","C","1","2","3"))

  # char_decreasing TRUE
  testthat::expect_equal(prep_char_num_sort(c("A","C","1",NA,NA,"3","B","2"),char_decreasing = TRUE),c("C","B","A","_NA","_NA","1","2","3"))
  # char_decreasing FALSE
  testthat::expect_equal(prep_char_num_sort(c("A","C","1",NA,NA,"3","B","2"),char_decreasing = FALSE),c("_NA","_NA","A","B","C","1","2","3"))
  # char_decreasing NULL
  testthat::expect_equal(prep_char_num_sort(c("A","C","1",NA,NA,"3","B","2"),char_decreasing = NULL),c("A","C", "_NA","_NA","B","1","2","3"))
  # char_decreasing NULL with only 1 numeric input.
  testthat::expect_equal(prep_char_num_sort(c(1),char_decreasing = NULL),c("1"))


  # num_decreasing TRUE
  testthat::expect_equal(prep_char_num_sort(c("A","C","1",NA,NA,"3","B","2"),num_decreasing = TRUE),c("_NA","_NA","A","B","C","3","2","1"))
  # num_decreasing FALSE
  testthat::expect_equal(prep_char_num_sort(c("A","C","1",NA,NA,"3","B","2"),num_decreasing = FALSE),c("_NA","_NA","A","B","C","1","2","3"))
  # num_decreasing NULL
  testthat::expect_equal(prep_char_num_sort(c("A","C","1",NA,NA,"3","B","2"),num_decreasing = NULL),c("_NA","_NA","A","B","C","1","3","2"))
  # num_decreasing NULL with only 1 input which is a string.
  testthat::expect_equal(prep_char_num_sort(c("A"),num_decreasing = NULL),c("A"))


})

testthat::test_that("Test errors when input is invalid",{

  # NA_val rejects inputs longer than 1.
  testthat::expect_error(prep_char_num_sort(c(1,2,3,"A"),NA_val = c(0,1)))

  # Enforces that array_to_sort is 1 dimensional vector
  testthat::expect_error(prep_char_num_sort(matrix(1:4, nrow = 2, nrow = 2), NA_val = NA))
  testthat::expect_error(prep_char_num_sort(data.frame(a = 1:4), NA_val = NA))

  # Do not allow NA_val to be TRUE/FALSE
  testthat::expect_error(prep_char_num_sort(c(1,2,3,"NA"), NA_val = TRUE))

  # Will error on having non-logical inputs for num_decreasing and char_decreasing.
  testthat::expect_error(prep_char_num_sort(c(1,2,3,"NA"), num_decreasing = 2))
  testthat::expect_error(prep_char_num_sort(c(1,2,3,"NA"), char_decreasing = 1))
  testthat::expect_error(prep_char_num_sort(c(1,2,3,"NA"), char_decreasing = "TRUE"))

  # Also issues for unique being non-logical
  testthat::expect_error(prep_char_num_sort(c(1,2,3,"NA"), unique_val = 1))
  testthat::expect_error(prep_char_num_sort(c(1,2,3,"NA"), unique_val = "TRUE"))

})

testthat::test_that("Examples:",{
  testthat::expect_equal(prep_char_num_sort(c("100","C","M","T","-55555","-44444","-33333","0",NA), NA_val = NA),
                                            c(NA, "C", "M", "T", "-55555", "-44444", "-33333", "0" , "100"))
  testthat::expect_equal(prep_char_num_sort(c("100","C","M","T","-55555","-44444","-33333","0",NA), NA_val = "-1"),
                                            c("C", "M", "T", "-55555", "-44444", "-33333", "-1", "0" , "100"))

})
