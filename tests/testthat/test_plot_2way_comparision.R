testthat::context("Testing relative lift curve plot. Checking error or no error")

testthat::test_that("Testing actuals and predicted  - Should error",{

  # Check lengths
  #Check actual, incumbent_pred and weights errors
  testthat::expect_error(plot_2way_comparison(actual=1:90, incumbent_pred=1:100, proposed_pred=1:100))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:90, proposed_pred=1:100))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:90))

  #Check valid values
  testthat::expect_error(plot_2way_comparison(actual=NULL, incumbent_pred=1:100, proposed_pred=1:100))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=NULL, proposed_pred=1:100))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=NULL))

  testthat::expect_error(plot_2way_comparison(actual=NA, incumbent_pred=1:100, proposed_pred=1:100))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=NA, proposed_pred=1:100))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=NA))

  testthat::expect_error(plot_2way_comparison(actual=rep("1",100), incumbent_pred=1:100, proposed_pred=1:100))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=rep("1",100), proposed_pred=1:100))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=rep("1",100)))

  testthat::expect_error(plot_2way_comparison(actual=c(1:99,NA), incumbent_pred=1:100, proposed_pred=1:100))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=c(1:99,NA), proposed_pred=1:100))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=c(1:99,NA)))


})

testthat::test_that("Testing weight - Should error",{
  # Check lengths
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, weight=1:90))

  # Check valid values
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, weight=NA))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, weight=rep("a",100)))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, weight=rep(-1,100)))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, weight=NULL))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, weight=c(1:99,NA)))
})


testthat::test_that("Testing binning params - Should error",{

  #Check n_bins
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, n_bins="bad"))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, n_bins=101))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, n_bins=10.5))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, n_bins=-1))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, n_bins=c(10, 10)))
  testthat::expect_warning(plot_2way_comparison(actual=1:100, incumbent_pred=1:100, proposed_pred=1:100, n_bins=3))

})

testthat::test_that("Other params give error when wrong type", {

  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), incumbent_label=1))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), proposed_label=1))

  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), rebase = "FALSE"))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), min_weight = -1))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), min_weight = 100))
  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), min_weight = c(1,2)))

})


testthat::test_that("Testing the function runs without errors",{

  #Check plots have no errors
  testthat::expect_true(plot_2way_comparison(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10)) %>% length()==2)
  testthat::expect_error(plot_2way_comparison(actual=0.1 * 1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), rebase = FALSE), NA)
  testthat::expect_error(plot_2way_comparison(actual=0.1 * 1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), rebase = TRUE), NA)

  testthat::expect_error(plot_2way_comparison(actual=1:100, incumbent_pred = 1:100 + rnorm(100, mean=0, sd = 25), proposed_pred = 1:100 + rnorm(100, mean=0, sd = 10), n_bins =  4), NA)



})
