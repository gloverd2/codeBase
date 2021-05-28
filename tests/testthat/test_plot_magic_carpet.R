testthat::context("Testing relative lift curve plot. Checking error or no error")

testthat::test_that("Testing actuals and predicted  - Should error",{

  #Check plots have no errors
  incumbent_pred <- 100 + 1:100 + rnorm(100, mean=0, sd = 25)
  proposed_pred <- 100 +  1:100 + rnorm(100, mean=0, sd = 10)

  testthat::expect_error(plot_magic_carpet(feature = 1:101, feature_name="feature_name", incumbent_pred = incumbent_pred, proposed_pred = proposed_pred))
  testthat::expect_error(plot_magic_carpet(feature = 1:100, feature_name="feature_name", incumbent_pred = incumbent_pred[1:99], proposed_pred = proposed_pred))
  testthat::expect_error(plot_magic_carpet(feature = 1:100, feature_name="feature_name", incumbent_pred = incumbent_pred, proposed_pred = proposed_pred[1:99]))

  testthat::expect_error(plot_magic_carpet(feature = c(1:99, NA), feature_name="feature_name", incumbent_pred = incumbent_pred, proposed_pred = proposed_pred))
  testthat::expect_error(plot_magic_carpet(feature = 1:100, feature_name="feature_name", incumbent_pred = c(incumbent_pred, NA), proposed_pred = c(proposed_pred, 10)))
  testthat::expect_error(plot_magic_carpet(feature = 1:100, feature_name="feature_name", incumbent_pred = c(incumbent_pred, 10), proposed_pred = c(proposed_pred, NA)))
  testthat::expect_error(plot_magic_carpet(feature = 1:100, feature_name="feature_name", incumbent_pred = c(incumbent_pred, -1), proposed_pred = c(proposed_pred, 10)))
  testthat::expect_error(plot_magic_carpet(feature = 1:100, feature_name="feature_name", incumbent_pred = c(incumbent_pred, 10), proposed_pred = c(proposed_pred, -1)))


  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=rep("1",100), proposed_pred=1:100))
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=1:100, proposed_pred=rep("1",100)))


})

testthat::test_that("Testing weight - Should error",{
  # Check lengths
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=1:100, proposed_pred=1:100, weight=1:90))

  # Check valid values
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=1:100, proposed_pred=1:100, weight=NA))
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=1:100, proposed_pred=1:100, weight=rep("a",100)))
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=1:100, proposed_pred=1:100, weight=rep(-1,100)))
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=1:100, proposed_pred=1:100, weight=NULL))
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=1:100, proposed_pred=1:100, weight=c(1:99,NA)))
})


testthat::test_that("Testing binning params - Should error",{

  #Check n_bins
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=1:100, proposed_pred=1:100, n_bins="bad"))
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=1:100, proposed_pred=1:100, n_bins=10.5))
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=1:100, proposed_pred=1:100, n_bins=-1))
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=1:100, proposed_pred=1:100, n_bins=c(10, 10)))


})

testthat::test_that("Other params give error when wrong type", {

  #Check plots have no errors
  incumbent_pred <- 100 + 1:100 + rnorm(100, mean=0, sd = 25)
  proposed_pred <- 100 +  1:100 + rnorm(100, mean=0, sd = 10)

  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=incumbent_pred, proposed_pred=proposed_pred, ratio_max = "d"))
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=incumbent_pred, proposed_pred=proposed_pred, ratio_step = "d"))
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=incumbent_pred, proposed_pred=proposed_pred, ratio_max=c(1,1)))
  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=incumbent_pred, proposed_pred=proposed_pred, ratio_step = c(0.1, 0.2)))


  testthat::expect_error(plot_magic_carpet(feature=1:100, feature_name="feature_name", incumbent_pred=incumbent_pred, proposed_pred=proposed_pred, position = "other"))
})


testthat::test_that("Testing the function runs without errors",{

  #Check plots have no errors
  incumbent_pred <- 100 + 1:100 + rnorm(100, mean=0, sd = 25)
  proposed_pred <- 100 +  1:100 + rnorm(100, mean=0, sd = 10)

  testthat::expect_error(plot_magic_carpet(feature = 1:100, feature_name="feature_name", incumbent_pred = incumbent_pred, proposed_pred = proposed_pred), NA)
  testthat::expect_error(plot_magic_carpet(feature = 1:100, feature_name="feature_name", incumbent_pred = incumbent_pred, proposed_pred = proposed_pred, position = "stack"), NA)

  testthat::expect_error(plot_magic_carpet(feature = 1:100, feature_name="feature_name", incumbent_pred = incumbent_pred, proposed_pred = proposed_pred, ratio_max = 0.5, ratio_step = 0.05), NA)


  #Check it looks ok when there is a big difference
  testthat::expect_error(plot_magic_carpet(feature = 1:100, feature_name="feature_name", incumbent_pred = incumbent_pred, proposed_pred = proposed_pred / 2, position = "stack"), NA)



})
