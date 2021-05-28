testthat::context("Testing plot_feature_predictions plot. Checking error or no error")


testthat::test_that("Testing feature - Should error",{
  #Will run
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep(1,10)), NA)

  #Should error
  testthat::expect_error(plot_feature_predictions(feature=NULL, feature_name="feature", actual=rep(1,10), prediction=rep(1,10)))
  testthat::expect_error(plot_feature_predictions(feature=NA, feature_name="feature", actual=rep(1,10), prediction=rep(1,10)))
  testthat::expect_error(plot_feature_predictions(feature=rep("1",10), feature_name="feature", actual=rep(1,10), prediction=rep(1,10)))
  testthat::expect_error(plot_ALE(data = 1:100, model_lm, explain_col="x1", n_bins=5))
})

testthat::test_that("Testing actual and predicted - Should error",{

  #Will run
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep(1,10)), NA)

  #Should error
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,9), prediction=rep(1,10)))
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep("1",10), prediction=rep(1,10)))
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=NA, prediction=rep(1,10)))
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=NULL, prediction=rep(1,10)))

  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep(1,9)))
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep("1",10)))
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=NA))
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=NULL))

})

testthat::test_that("Testing n_bins - Should error",{
  # Testing type
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep(1,10), n_bins=5), NA)
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep(1,10), n_bins=NA))
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep(1,10), n_bins=-1))
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep(1,10), n_bins="5"))
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep(1,10), n_bins=5.5))

})


testthat::test_that("Testing weight - Should error",{
  # Testing type
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep(1,10), weight=rep(-1,10)))
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep(1,10), weight=rep("1",10)))
  testthat::expect_error(plot_feature_predictions(feature=1:10, feature_name="feature", actual=rep(1,10), prediction=rep(1,10), weight=NULL))

})


