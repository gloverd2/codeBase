testthat::context("Testing PDP plot. Checking error or no error")

data <- data.frame(x1=runif(100, 0, 25), x2=runif(100, 0, 25), x3=runif(100, 0, 25), x4=sample(1:10, size=100, replace = TRUE)) %>%
  dplyr::mutate(target=x1^2 * 0.01 + x2 + x4 + rnorm(dplyr::n(),sd=5))

model_lm <- glm(target ~ poly(x1, 2) + x2 + x3 + x4, data=data)

model_gbm <- xgboost::xgboost(data = as.matrix(data[,which(!(names(data)=="target"))]), label=data[["target"]], nrounds=20, verbose = 0)


testthat::test_that("Testing Data - Should error",{
  testthat::expect_error(plot_PDP(NULL, model_lm, explain_col="x1", n_bins=5))
  testthat::expect_error(plot_PDP(NA, model_lm, explain_col="x1", n_bins=5))
  testthat::expect_error(plot_PDP("data", model_lm, explain_col="x1", n_bins=5))
  testthat::expect_error(plot_PDP(1:100, model_lm, explain_col="x1", n_bins=5))
})

testthat::test_that("Testing explain_col - Should error",{
  # Testing column must be in data
  testthat::expect_error(plot_PDP(data, model_lm, explain_col=NULL, n_bins=5))
  testthat::expect_error(plot_PDP(data, model_lm, explain_col=NA, n_bins=5))
  testthat::expect_error(plot_PDP(data, model_lm, explain_col="y1", n_bins=5))
  testthat::expect_error(plot_PDP(data, model_lm, weight=rep(1,100), explain_col=c("x1","y1"), n_bins=5))
  testthat::expect_error(plot_PDP(data, model_lm, weight=rep(1,100), explain_col=c("x1","x1"), n_bins=5))
  # Must be length <2
  testthat::expect_error(plot_PDP(data, model_lm, weight=rep(1,100), explain_col=c("x1","x2","x3"), n_bins=5))
  # Must be string
  testthat::expect_error(plot_PDP(data, model_lm, weight=rep(1,100), explain_col=1, n_bins=5))
})

testthat::test_that("Testing n_bins - Should error",{
  # Testing type
  testthat::expect_error(plot_PDP(data, model_lm, explain_col="x1", n_bins=NA))
  testthat::expect_error(plot_PDP(data, model_lm, explain_col="x1", n_bins=-1))
  testthat::expect_error(plot_PDP(data, model_lm, explain_col="x1", n_bins=10.5))
  testthat::expect_error(plot_PDP(data, model_lm, explain_col="x1", n_bins="10"))
  # n_bins can only be length 2 if explain_col is length 2
  testthat::expect_error(plot_PDP(data, model_lm, explain_col="x1", n_bins=c(10,10)))
  # n_bins can only be length 1 or 2
  testthat::expect_error(plot_PDP(data, model_lm, explain_col=c("x1","x2"), n_bins=c(10,10,10)))
})


testthat::test_that("Testing weight - Should error",{
  # Testing type
  testthat::expect_error(plot_PDP(data, model_lm, weight=rep("1",100), explain_col="x1", n_bins=5))
  testthat::expect_error(plot_PDP(data, model_lm, weight=rep(1,99), explain_col="x1", n_bins=5))
  testthat::expect_error(plot_PDP(data, model_lm, weight=1, explain_col="x1", n_bins=5))
  testthat::expect_error(plot_PDP(data, model_lm, weight=NULL, explain_col="x1", n_bins=5))
})


testthat::test_that("Testing the function runs without errors",{
  #Should run with no errors
  testthat::expect_error(plot_PDP(data, model_lm, explain_col="x1"), NA)
  testthat::expect_error(plot_PDP(data, model_lm, explain_col="x2"), NA)
  testthat::expect_error(plot_PDP(data, model_lm, explain_col="x3"), NA)
  testthat::expect_error(plot_PDP(data, model_lm, explain_col="x4"), NA)
  testthat::expect_error(plot_PDP(data, model_lm, explain_col="x4", use_plotly=FALSE), NA)
  testthat::expect_error(plot_PDP(data, model_lm, weight=rep(1,100), explain_col=c("x1", "x2"), n_bins=5), NA)
  testthat::expect_error(plot_PDP(data, model_lm, weight=rep(1,100), explain_col=c("x1", "x2"), n_bins=5, use_plotly=FALSE), NA)

  testthat::expect_error(plot_PDP(data[,which(!(names(data)=="target"))], model_gbm, explain_col="x1", n_bins=5), NA)
  testthat::expect_error(plot_PDP(data[,which(!(names(data)=="target"))], model_gbm, explain_col="x2", n_bins=7), NA)
  testthat::expect_error(plot_PDP(data[,which(!(names(data)=="target"))], model_gbm, explain_col="x4", n_bins=7), NA)
  testthat::expect_error(plot_PDP(data[,which(!(names(data)=="target"))], model_gbm, explain_col="x4", n_bins=7, use_plotly=FALSE), NA)
  testthat::expect_error(plot_PDP(data[,which(!(names(data)=="target"))], model_gbm, weight=rep(1,100), explain_col=c("x1", "x2"), n_bins=c(5,10)), NA)
  testthat::expect_error(plot_PDP(data[,which(!(names(data)=="target"))], model_gbm, weight=rep(1,100), explain_col=c("x1", "x2"), n_bins=c(5,10),  use_plotly=FALSE), NA)

})
