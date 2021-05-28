testthat::context("Testing relative lift curve plot. Checking error or no error")

testthat::test_that("Testing actuals and predicted  - Should error",{

  # Check lengths
  #Check actual, incumbent_pred and weights errors
  testthat::expect_error(plot_lift_curve_relative(actual=1:90, incumbent_pred=1:100, proposed_pred=1:100))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:90, proposed_pred=1:100))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=1:90))

  #Check valid values
  testthat::expect_error(plot_lift_curve_relative(actual=NULL, incumbent_pred=1:100, proposed_pred=1:100))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=NULL, proposed_pred=1:100))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=NULL))

  testthat::expect_error(plot_lift_curve_relative(actual=NA, incumbent_pred=1:100, proposed_pred=1:100))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=NA, proposed_pred=1:100))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=NA))

  testthat::expect_error(plot_lift_curve_relative(actual=rep("1",100), incumbent_pred=1:100, proposed_pred=1:100))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=rep("1",100), proposed_pred=1:100))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=rep("1",100)))


})

testthat::test_that("Testing weight - Should error",{
  # Check lengths
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, weight=1:90))

  # Check valid values
  testthat::expect_error(plot_lift_curve(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, weight=NA))
  testthat::expect_error(plot_lift_curve(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, weight=rep("a",100)))
  testthat::expect_error(plot_lift_curve(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, weight=rep(-1,100)))
  testthat::expect_error(plot_lift_curve(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, weight=NULL))
})


testthat::test_that("Testing binning params - Should error",{
  pred <- 1:100 + rnorm(100, mean=0, sd = 10)

  #Check option errors
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, method="Bad"))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=pred, method="gaussian_weight", mean=-1))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=pred, method="gaussian_weight", mean=2))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=pred, method="gaussian_weight", mean=0.5, sd=10))

  #Check use_labels
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=pred, use_labels="bad"))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=pred, use_labels=NA))

  #Check n_bins
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=pred, n_bins="bad"))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=pred, n_bins=101))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=pred, n_bins=10.5))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=pred, n_bins=-1))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred=1:100, proposed_pred=pred, n_bins=c(10, 10)))

})

testthat::test_that("Other params give error when wrong type", {

  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), title=1))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), incumbent_label=1))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), proposed_label=1))

  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), use_plotly = NA))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), use_plotly = NULL))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), use_plotly = "bad"))

  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), rebase = NA))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), rebase = NULL))
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), rebase = "bad"))
})


testthat::test_that("Testing the function runs without errors",{

  #Check plots have no errors
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), title="Example Lift Curve"), NA)
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), title="Example Lift Curve", use_labels = FALSE), NA)
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), method="gaussian_weight", title="Example Lift Curve - skew even"), NA)
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), method="gaussian_weight", title="Example Lift Curve - skew", mean=0, sd=0.4), NA)
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), method="gaussian_weight", title="Example Lift Curve - skew", mean=1, sd=0.4), NA)
  testthat::expect_error(plot_lift_curve_relative(actual=1:100 * 0.1, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), rebase = TRUE), NA)

  #Check ggplot version
  testthat::expect_error(plot_lift_curve_relative(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), title="Example Lift Curve", use_plotly=FALSE), NA)
})
