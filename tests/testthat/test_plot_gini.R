testthat::context("Testing gini plot. Checking error or no error")

testthat::test_that("Testing actuals and predicted  - Should error",{

  # Check lengths
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:9))

  #Check valid values
  testthat::expect_error(plot_gini(actual=1:10, predicted=NULL))
  testthat::expect_error(plot_gini(actual=1:10, predicted=NA))
  testthat::expect_error(plot_gini(actual=1:10, predicted=rep("a",10)))

  testthat::expect_error(plot_gini(actual=NULL, predicted=1:10))
  testthat::expect_error(plot_gini(actual=NA, predicted=1:10))
  testthat::expect_error(plot_gini(actual=rep("a",10), predicted=1:10))

})


testthat::test_that("Testing weight - Should error",{
  # Check lengths
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, weight=1:9))

  # Check valid values
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, weight=NA))
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, weight=rep("a",10)))
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, weight=rep(-1,10)))
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, weight=NULL))
})

testthat::test_that("Testing use_plotly - Should error",{

  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, weight=1:10, use_plotly = NA))
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, weight=1:10, use_plotly = NULL))
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, weight=1:10, use_plotly = "0"))
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, use_plotly = c(TRUE,TRUE)))

})

testthat::test_that("Testing NA behaviour",{

  #Should error
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, na.rm = "TRUE"))
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, na.rm = c(TRUE,TRUE)))

  #Testing warnings
  testthat::expect_warning(plot_gini(actual=c(1:10,NA), predicted=c(1:10,NA))) # Should give warning about NAs
  testthat::expect_warning(plot_gini(actual=c(1:10,NA), predicted=c(1:10,NA), na.rm = TRUE),NA) # Shouldn't give warning about NAs
})


testthat::test_that("Testing the function runs without errors",{
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10), NA)
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, weight=rep(1,10)), NA)
  testthat::expect_error(plot_gini(actual=1:10, predicted=rep(0.5,10)), NA)
  testthat::expect_error(plot_gini(actual=1:10, predicted=1:10, use_plotly = FALSE), NA)

  testthat::expect_error(plot_gini(actual=1:10, predicted=data.frame(p1=1:10, p2=rnorm(10))), NA)
  testthat::expect_error(plot_gini(actual=1:10, predicted=data.frame(p1=1:10, p2=rnorm(10)), weight=rep(1,10)), NA)
  testthat::expect_error(plot_gini(actual=1:10, predicted=data.frame(p1=1:10, p2=rnorm(10)), weight=rep(1,10), use_plotly = FALSE), NA)
})
