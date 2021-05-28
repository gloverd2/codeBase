
testthat::test_that("Testing numeric caps",{


  # Single vector input -----------------------------------------------------

  # Simple test
  data_in <- 1:10
  testthat::expect_equal(prep_numeric_caps(data_in, cap_below=3), c(rep(3,3), 4:10))
  testthat::expect_equal(prep_numeric_caps(data_in, cap_above=7), c(1:6 , rep(7,4)))
  testthat::expect_equal(prep_numeric_caps(data_in, cap_below=3, cap_above=7), c(rep(3,3), 4:6 , rep(7,4)))

  testthat::expect_equal(prep_numeric_caps(data_in, unknown_below=4), c(rep(NA,3), 4:10))
  testthat::expect_equal(prep_numeric_caps(data_in, unknown_above=6), c(1:6 , rep(NA,4)))
  testthat::expect_equal(prep_numeric_caps(data_in, unknown_below=4, unknown_above=6), c(rep(NA,3), 4:6 , rep(NA,4)))

  testthat::expect_error(prep_numeric_caps(data_in, cap_below=4 , unknown_below=5))
  testthat::expect_error(prep_numeric_caps(data_in, cap_above=5 , unknown_above=4))
  testthat::expect_error(prep_numeric_caps(data_in, cap_below=5 , cap_above=4))
  testthat::expect_error(prep_numeric_caps(data_in, unknown_below=5 , unknown_above=4))


  # dataframe input ---------------------------------------------------------

  data_in_df <- data.frame(
    first=seq(0,10,1),
    second=seq(0,100,10),
    third=seq(0,10,1)
  )

  cap_below <- list(first=3, second = 30, third=NULL)
  cap_above <- list(first=7, second = 70, third=NULL)
  unknown_below <- list(first=1, second = 10, third=NULL)
  unknown_above <- list(first=9, second = 90, third=NULL)

  data_out <- prep_numeric_caps(data=data_in_df, cap_below=cap_below, cap_above=cap_above, unknown_below=unknown_below, unknown_above=unknown_above)
  testthat::expect_equal(data_out[["first"]], c(NA, 3, 3, 3, 4, 5, 6, 7, 7, 7, NA))
  testthat::expect_equal(data_out[["second"]], c(NA, 30, 30, 30, 40, 50, 60, 70, 70, 70, NA))
  testthat::expect_equal(data_out[["third"]], seq(0,10,1))

})


