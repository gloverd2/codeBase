testthat::context("Testing lift curve plot. Checking error or no error")


testthat::test_that("Testing actuals and predicted  - Should error",{

  # Check lengths
  testthat::expect_error(plot_lift_curve(actual=1:10, predicted=1:9))

  #Check valid values
  testthat::expect_error(plot_lift_curve(actual=1:10, predicted=NULL))
  testthat::expect_error(plot_lift_curve(actual=1:10, predicted=NA))
  testthat::expect_error(plot_lift_curve(actual=1:10, predicted=rep("a",10)))

  testthat::expect_error(plot_lift_curve(actual=NULL, predicted=1:10))
  testthat::expect_error(plot_lift_curve(actual=NA, predicted=1:10))
  testthat::expect_error(plot_lift_curve(actual=rep("a",10), predicted=1:10))

})


testthat::test_that("Testing weight - Should error",{
  # Check lengths
  testthat::expect_error(plot_lift_curve(actual=1:10, predicted=1:10, weight=1:9))

  # Check valid values
  testthat::expect_error(plot_lift_curve(actual=1:10, predicted=1:10, weight=NA))
  testthat::expect_error(plot_lift_curve(actual=1:10, predicted=1:10, weight=rep("a",10)))
  testthat::expect_error(plot_lift_curve(actual=1:10, predicted=1:10, weight=rep(-1,10)))
  testthat::expect_error(plot_lift_curve(actual=1:10, predicted=1:10, weight=NULL))
})


testthat::test_that("Testing binning params - Should error",{
  pred <- 1:100 + rnorm(100, mean=0, sd = 10)

  #Check option errors
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, method="Bad"))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, method="gaussian_weight", mean=-1))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, method="gaussian_weight", mean=2))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, method="gaussian_weight", mean=0.5, sd=10))

  #Check use_labels
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, use_labels="bad"))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, use_labels=NA))

  #Check n_bins
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, n_bins="bad"))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, n_bins=101))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, n_bins=10.5))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, n_bins=-1))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, n_bins=c(10, 10)))

})


testthat::test_that("Other params give error when wrong type", {

  pred <- 1:100 + rnorm(100, mean=0, sd = 10)

  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, title=1))

  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, use_plotly = NA))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, use_plotly = NULL))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, use_plotly = "bad"))

  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, rebase = NA))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, rebase = NULL))
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, rebase = "bad"))
})


testthat::test_that("Testing the function runs without errors",{

  pred <- 1:100 + rnorm(100, mean=0, sd = 10)

  #Check plots have no errors
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, title="Example Lift Curve"), NA)
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, title="Example Lift Curve", use_labels = FALSE), NA)
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, method="gaussian_weight", title="Example Lift Curve - skew even"), NA)
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, method="gaussian_weight", title="Example Lift Curve - skew", mean=0, sd=0.4), NA)
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, method="gaussian_weight", title="Example Lift Curve - skew", mean=1, sd=0.4), NA)
  testthat::expect_error(plot_lift_curve(actual=1:100 * 0.1, predicted = pred, rebase = TRUE), NA)

  #Check ggplot version
  testthat::expect_error(plot_lift_curve(actual=1:100, predicted = pred, title="Example Lift Curve", use_plotly = FALSE), NA)
})
