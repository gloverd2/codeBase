testthat::context("Testing prep_num_band - A numeric banding function")

testthat::test_that("Testing properties",{

  # Can handle characters
  testthat::expect_equal(prep_num_band(c("A","B","1","2","C","5","6","7"),c(2,8)),c("A","B","2","2","C","8","8","8"))

  # Character only input
  testthat::expect_equal(prep_num_band(c("A","B","C","D"),c(1,2,3)),c("A","B","C","D"))

  # Can handle NAs
  testthat::expect_equal(prep_num_band(c(1,NA,2,3,4,5,NA),c(2,6)),c(2,NA,2,6,6,6,NA))

  # Can handle negative bandings
  testthat::expect_equal(prep_num_band(c(-9,-12,-2,-45,1,0,-1,-4),c(-10,-5,-2,2,6)),c(-5,-10,-2,-10,2,2,2,-2))

  # Warning condition on just 1 unique value calls
  testthat::expect_warning(
    testthat::expect_equal(prep_num_band(c(1),c(2,3)),c(2))
    ) # Returns a warning.

  # Can handle non-integer
  testthat::expect_equal(prep_num_band(c(-1.1,2.3,1.5,7.8,1.45,62.76,-4.26,-5.1),c(-3.2,-0.9,2.8,5.2,10.8)),c(-0.9,2.8,2.8,10.8,2.8,99999999,-3.2,-3.2))

  # Edge cases
  testthat::expect_equal(prep_num_band(c(4.9999999,5,5.00000001),c(5,10)),c(5,5,10))

  # Can handle values above out_of_bands.
  testthat::expect_warning(
    testthat::expect_equal(prep_num_band(c(1,10,463),c(1,2,3,100,1000),out_of_bands = 999),c(1,100,1000),label = "highest bands value is above out_of_bands") # Returns a warning
  )
  testthat::expect_warning(
    testthat::expect_equal(prep_num_band(c(1,4,1000),c(1,2,3,100),out_of_bands = 999),c(1,100,1000),label = "highest var_to_band value is above out_of_bands") # Returns a warning
  )

  # Can handle infinite values
  testthat::expect_warning(
    testthat::expect_equal(prep_num_band(c(1,10,Inf),c(1,2,3,100),out_of_bands = 999),c(1,100,Inf),label = "highest var_to_band value is infinite") # Returns a warning
  )
  testthat::expect_warning(
    testthat::expect_equal(prep_num_band(c(1,10,101),c(1,2,3,10,Inf),out_of_bands = 999),c(1,10,Inf),label = "highest bands value is above out_of_bands") # Returns a warning
  )
  testthat::expect_equal(prep_num_band(c(1,10,99),c(1,2,3,10),out_of_bands = Inf),c(1,10,Inf),label = "out_of_bands value is above out_of_bands")
  testthat::expect_equal(prep_num_band(c(1,10,-Inf),c(1,2,3,100),out_of_bands = 999),c(1,100,1),label = "lowest var_to_band value is minus infinite")
  testthat::expect_equal(prep_num_band(c(-Inf,1,10),c(-Inf,1,2,3,10),out_of_bands = 999),c(-Inf,1,10),label = "lowest bands value is minus infinite")
  testthat::expect_warning(
    testthat::expect_equal(prep_num_band(c(1,10,99),c(1,2,3,10),out_of_bands = -Inf),c(1,10,99),label = "out_of_bands value is minus infinite") # Returns a warning
  )

  # Testing the indices option
  testthat::expect_equal(prep_num_band(var_to_band = c("A","B",1,2,3,4,5,6,7,8), bands = c(3,5,7), out_of_bands = 999, output = "indices"),c(NA,NA,1,1,1,2,2,3,3,4))
  testthat::expect_warning(
    testthat::expect_equal(prep_num_band(var_to_band = c("A","B",1,2,3,4,5,6,7,9999), bands = c(3,5,7), out_of_bands = 999, output = "indices"),c(NA,NA,1,1,1,2,2,3,3,4))
  )
})

testthat::test_that("Test errors when input is invalid",{

  # NULL inputs
  testthat::expect_error(prep_num_band(c(),c(1,3)),label = "var_to_band is NULL")
  testthat::expect_error(prep_num_band(c(1,2,3),c()),label = "bands is NULL")
  testthat::expect_error(prep_num_band(c(1,2,3,4),c(2),out_of_bands = NULL), label = "out_of_bands is NULL")

  # Invalid Inputs for bands
  testthat::expect_error(prep_num_band(c(1,2,3,"A"),c("A","B")),label = "bands contains character entries")
  testthat::expect_error(prep_num_band(c(1,2,3,5,6,"A"),c(1,2,NA)),label = "bands contains NAs")

  # Invalid Inputs for max_limit
  testthat::expect_error(prep_num_band(c(1,2,3,"A"),c(1,2),out_of_bands = "A"),label = "out_of_bands contains character entries")
  testthat::expect_error(prep_num_band(c(1,2,3,5,6,"A"),c(1,2),out_of_bands = NA),label = "out_of_bands contains NAs")

  # Bands is unsorted
  testthat::expect_error(prep_num_band(c(1,2,3,"A"),c(2,1),out_of_bands = 999),label = "bands is out of order")

})

testthat::test_that("Examples:",{
  testthat::expect_equal(prep_num_band(c(-10,-5,-3,-2,4),c(-3,-3,-3,1,5)),c(-3,-3,-3,1,5))
  testthat::expect_equal(prep_num_band(c("A","B","1","C","5.5"),c(2,8)),c("A","B","2","C","8"))

})
