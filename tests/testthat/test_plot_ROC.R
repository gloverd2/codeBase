testthat::context("Testing ROC curve plot. Checking error or no error")

testthat::test_that("Testing actuals and predicted  - Should error",{

  # Check lengths
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,9)))

  #Check valid values
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=NULL))
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=NA))
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep("a",10)))
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=c(rep(-0.1,5),rep(1,5))))
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=c(rep(0,5),rep(1.1,5))))


  testthat::expect_error(plot_ROC(actual=NULL, predicted=rep(0.5,10)))
  testthat::expect_error(plot_ROC(actual=NA, predicted=rep(0.5,10)))
  testthat::expect_error(plot_ROC(actual=rep("a",10), predicted=rep(0.5,10)))
  testthat::expect_error(plot_ROC(actual=rep(-1,10), predicted=rep(0.5,10)))
  testthat::expect_error(plot_ROC(actual=rep(2,10), predicted=rep(0.5,10)))
  testthat::expect_error(plot_ROC(actual=rep(0.5,10), predicted=rep(0.5,10)))

})


testthat::test_that("Testing weight - Should error",{
  # Check lengths
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=1:9))

  # Check valid values
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=NA))
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=rep("a",10)))
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=rep(-1,10)))
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=NULL))
})


testthat::test_that("Testing plotly logic - Should error",{
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), use_plotly = "TRUE"))
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), use_plotly = c(TRUE,TRUE)))
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), use_plotly = NA))
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), use_plotly = NULL))
})


testthat::test_that("Testing NA behaviour",{

  #Should error
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), na.rm = "TRUE"))
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), na.rm = c(TRUE,TRUE)))

  #Testing warnings
  testthat::expect_warning(plot_ROC(actual=c(rep(0,5),rep(1,5),NA), predicted=rep(0.5,11))) # Should give warning about NAs
  testthat::expect_warning(plot_ROC(actual=c(rep(0,5),rep(1,5),NA), predicted=rep(0.5,11), na.rm = TRUE),NA) # Shouldn't give warning about NAs
})


testthat::test_that("Testing the function runs without errors",{
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10)), NA)
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=rep(1,10)), NA)
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=data.frame(p1=c(rep(0,5),rep(1,5)), p2=rep(0.5,10))), NA)
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=data.frame(p1=c(rep(0,5),rep(1,5)), p2=rep(0.5,10)), weight=rep(1,10)), NA)
  testthat::expect_error(plot_ROC(actual=c(rep(0,5),rep(1,5)), predicted=data.frame(p1=c(rep(0,5),rep(1,5)), p2=rep(0.5,10)), weight=rep(1,10), use_plotly = FALSE), NA)
})
