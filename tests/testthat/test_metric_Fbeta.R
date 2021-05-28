testthat::context("Testing metrics - Fbeta")

testthat::test_that("Testing metric properties", {

  v1 <- c(rep(0,5), rep(1,5))
  v2 <- seq(0,1, length.out=10)
  weight1 <- rep(1, 10)
  weight2 <- pmax(0, rnorm(n = 10, mean = 10, sd = 1))


  # Check metric is self is 0
  testthat::expect_equal(metric_Fbeta(v1, v1, weight1) , 1, label = "Check value when all predictions are correct")
  testthat::expect_equal(metric_Fbeta(v1, v2, weight2) , 1, label = "Check value when all predictions are correct")
  testthat::expect_equal(metric_Fbeta(v1, seq(1,0, length.out=10), weight2) , 0, label = "Check value when all predictions are wrong")


})



testthat::test_that("Test errors when input is invalid - lengths",{

  # Check lengths of inputs
  testthat::expect_error(metric_Fbeta(actual=c(0,1,1), predicted=c(0,1)))
  testthat::expect_error(metric_Fbeta(actual=c(0,1,1), predicted=c(0,1,1), weight=c(1,1)))
})

testthat::test_that("Test errors when input is invalid - actuals",{
  testthat::expect_error(metric_Fbeta(actual=NULL, predicted=c(0, 1)))
  testthat::expect_error(metric_Fbeta(actual=NA, predicted=c(0, 1)))
  testthat::expect_error(metric_Fbeta(actual=c("a", "b"), predicted=c(0, 1)))
  testthat::expect_error(metric_Fbeta(actual=c(1, -1), predicted=c(0, 1)))
  testthat::expect_error(metric_Fbeta(actual=c(1, 2), predicted=c(0, 1)))
  testthat::expect_error(metric_Fbeta(actual=c(1, 0.5), predicted=c(0, 1)))

  testthat::expect_warning(metric_Fbeta(actual=c(1, 1), predicted=c(0, 1)), label="only single target")
})

testthat::test_that("Test errors when input is invalid - predicted",{
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=NULL))
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=NA))
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=c("a", "b")))
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=c(0, 2)))
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=c(0, -1)))
})

testthat::test_that("Test errors when input is invalid - weight",{
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=c(0, 1), weight=NA))
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=c(0, 1), weight=c("a", "b")))
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=c(0, 1), weight=c(1, -1)))
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=c(0, 1), weight=c(0, 0)))
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=c(0, 1), weight=NULL))

})

testthat::test_that("Test errors when input is invalid - Other",{
  # threshold must be >0 and <1
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=c(0, 1), weight=c(1, 1), threshold="True"))
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=c(0, 1), weight=c(1, 1), threshold=-0.1))
  testthat::expect_error(metric_Fbeta(actual=c(0, 1), predicted=c(0, 1), weight=c(1, 1), threshold=1.1))
})

testthat::test_that("Test errors when input is invalid - NAs in input",{

  # NA inputs
  testthat::expect_true(is.na(metric_Fbeta(actual=c(0, 1, NA), predicted=c(0, 1, 1))))
  testthat::expect_true(!is.na(sum(metric_Fbeta(actual=c(0 ,1, NA), predicted=c(0, 1, 1), na.rm=TRUE))))

  testthat::expect_equal(metric_Fbeta(actual=c(0, 1, NA), predicted=c(0, 1, NA), weight=c(0, 2, NA), na.rm=TRUE),
                         metric_Fbeta(actual=c(0, 1),     predicted=c(0, 1),     weight=c(0, 2),     na.rm=TRUE),
                         label = "Check NAs removed correctly")

})


testthat::test_that("Numeric example",{
  actual <- c(rep(0,50), rep(1,50))
  predicted <- seq(0, 1, length.out = 100)
  weight <- pmax(0,rnorm(100, mean=1, sd=0.1))

  testthat::expect_equal(metric_Fbeta(actual, predicted, weight), 1)

  precision <- metric_precision(actual, predicted, weight, threshold=0.25)
  recall <- metric_recall(actual, predicted, weight, threshold=0.25)
  testthat::expect_equal(metric_Fbeta(actual, predicted, weight, threshold=0.25), (2 * precision * recall) / (precision + recall))

  precision <- metric_precision(actual, predicted, weight, threshold=0.75)
  recall <- metric_recall(actual, predicted, weight, threshold=0.75)
  testthat::expect_equal(metric_Fbeta(actual, predicted, weight, threshold=0.75, beta=2), (5 * precision * recall) / ((4 * precision) + recall))

})
