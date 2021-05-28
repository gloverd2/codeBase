testthat::context("Testing ALE plot. Checking error or no error")

df <- data.frame(x1=runif(100, 0, 25), x2=runif(100, 0, 25), x3=runif(100, 0, 25)) %>%
  dplyr::mutate(target=x1^2 * 0.01 + x2 + rnorm(dplyr::n(),sd=1))

model_lm <- glm(target ~ poly(x1, 2) + x2, data=df)

model_gbm <- xgboost::xgboost(data = as.matrix(df[,which(!(names(df)=="target"))]), label=df[["target"]], nrounds=20, verbose = 0)


testthat::test_that("Testing Data - Should error",{
  testthat::expect_error(plot_ALE(data = NULL, model_lm, explain_col="x1", n_bins=5))
  testthat::expect_error(plot_ALE(data = NA, model_lm, explain_col="x1", n_bins=5))
  testthat::expect_error(plot_ALE(data = "data", model_lm, explain_col="x1", n_bins=5))
  testthat::expect_error(plot_ALE(data = 1:100, model_lm, explain_col="x1", n_bins=5))
})

testthat::test_that("Testing explain_col - Should error",{
  # Testing column must be in data
  testthat::expect_error(plot_ALE(data = df, model_lm, explain_col=NULL, n_bins=5))
  testthat::expect_error(plot_ALE(data = df, model_lm, explain_col=NA, n_bins=5))
  testthat::expect_error(plot_ALE(data = df, model_lm, explain_col="y1", n_bins=5))
  # Can't yet take two columns
  testthat::expect_error(plot_ALE(data = df, model_lm, weight=rep(1,100), explain_col=c("x1","y2"), n_bins=5))
  # Must be string
  testthat::expect_error(plot_ALE(data = df, model_lm, weight=rep(1,100), explain_col=1, n_bins=5))
})

testthat::test_that("Testing n_bins - Should error",{
  # Testing type
  testthat::expect_error(plot_ALE(data = df, model_lm, explain_col="x1", n_bins=NA))
  testthat::expect_error(plot_ALE(data = df, model_lm, explain_col="x1", n_bins=-1))
  testthat::expect_error(plot_ALE(data = df, model_lm, explain_col="x1", n_bins=10.5))
  testthat::expect_error(plot_ALE(data = df, model_lm, explain_col="x1", n_bins="10"))
})


testthat::test_that("Testing weight - Should error",{
  # Testing type
  testthat::expect_error(plot_ALE(data = df, model_lm, weight=rep("1",100), explain_col="x1", n_bins=5))
  testthat::expect_error(plot_ALE(data = df, model_lm, weight=rep(1,99), explain_col="x1", n_bins=5))
  testthat::expect_error(plot_ALE(data = df, model_lm, weight=1, explain_col="x1", n_bins=5))
  testthat::expect_error(plot_ALE(data = df, model_lm, weight=NULL, explain_col="x1", n_bins=5))
})


testthat::test_that("Testing the function runs without errors",{
  #Should run with no errors
  testthat::expect_error(plot_ALE(data = df, model_lm, explain_col="x1"), NA)
  testthat::expect_error(plot_ALE(data = df, model_lm, explain_col="x2"), NA)
  testthat::expect_error(plot_ALE(data = df, model_lm, explain_col="x2", use_plotly=FALSE), NA)

  testthat::expect_error(plot_ALE(data = df[,which(!(names(df)=="target"))], model_gbm, explain_col="x1", n_bins=5), NA)
  testthat::expect_error(plot_ALE(data = df[,which(!(names(df)=="target"))], model_gbm, explain_col="x2", n_bins=5), NA)
  testthat::expect_error(plot_ALE(data = df[,which(!(names(df)=="target"))], model_gbm, explain_col="x2", n_bins=5, use_plotly=FALSE), NA)

})

