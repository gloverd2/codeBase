testthat::context("Testing metrics - Root mean squared error")

testthat::test_that("Testing metric properties", {

  v1 <- rep(10, 10)
  v2 <- rnorm(n = 10, mean = 10, sd = 1)
  weight1 <- rep(1, 10)
  weight2 <- pmax(0, rnorm(n = 10, mean = 10, sd = 1))


  # Check metric is self is 0
  testthat::expect_equal(metric_rmse(v1, v1, weight1) ,0, label = "metric with self is 0")
  testthat::expect_equal(metric_rmse(v2, v2, weight2) ,0, label = "metric with self is 0")

  # Check symmetry
  testthat::expect_equal(metric_rmse(v1, v2, weight1) ,metric_rmse(v2, v1, weight1), label = "rmse is symmetric")
  testthat::expect_equal(metric_rmse(v1, v2, weight2) ,metric_rmse(v2, v1, weight2), label = "rmse is symmetric")

  # Check weights matter
  testthat::expect_false(isTRUE(all.equal(metric_rmse(v1, v2, weight1) ,metric_rmse(v1, v2, weight2))), label = "weight matters")

  # Check rebasing matters
  testthat::expect_equal(metric_rmse(actual=v1, predicted=v1, weight=weight1, rebase=FALSE), metric_rmse(actual=v1, predicted=v1+10, weight=weight1, rebase=TRUE))
  testthat::expect_equal(metric_rmse(actual=v2, predicted=v2, weight=weight2, rebase=FALSE), metric_rmse(actual=v2, predicted=v2+10, weight=weight2, rebase=TRUE))

})


testthat::test_that("Test errors when input is invalid - lenghts",{
  testthat::expect_error(metric_rmse(1:10, 1:9))
  testthat::expect_error(metric_rmse(1:10, 1:10, 1:9))
})

testthat::test_that("Test errors when input is invalid - actuals",{
  testthat::expect_error(metric_rmse(actual=NA, predicted=c(1, 2)))
  testthat::expect_error(metric_rmse(actual=NULL, predicted=c(1, 2)))
  testthat::expect_error(metric_rmse(actual=c("a", "b"), predicted=c(1, 2)))
})

testthat::test_that("Test errors when input is invalid - predicted",{
  testthat::expect_error(metric_rmse(actual=c(1, 2), predicted=NA))
  testthat::expect_error(metric_rmse(actual=c(1, 2), predicted=NULL))
  testthat::expect_error(metric_rmse(actual=c(1, 2), predicted=c("a", "b")))
})

testthat::test_that("Test errors when input is invalid - weight",{
  testthat::expect_error(metric_rmse(actual=c(1, 2), predicted=c(1, 2), weight=NA))
  testthat::expect_error(metric_rmse(actual=c(1, 2), predicted=c(1, 2), weight=c("a", "b")))
  testthat::expect_error(metric_rmse(actual=c(1, 2), predicted=c(1, 2), weight=c(-0.1, 1)))
  testthat::expect_error(metric_rmse(actual=c(1, 2), predicted=c(1, 2), weight=c(0, 0)))
  testthat::expect_error(metric_rmse(actual=c(1, 2), predicted=c(1, 2), weight=NULL))
})

testthat::test_that("Test errors when input is invalid - Other",{

  # na.rm and rebase must be logical
  testthat::expect_error(metric_rmse(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), na.rm="True"))
  testthat::expect_error(metric_rmse(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), rebase="True"))

  # NA inputs
  testthat::expect_true(is.na(metric_rmse(actual=c(1, 2, NA), predicted=c(1, 2, 3))))
  testthat::expect_true(!is.na(metric_rmse(actual=c(1 ,2, NA), predicted=c(1, 2, 3), na.rm=TRUE)))

  testthat::expect_equal(metric_rmse(actual=c(1, 2, NA), predicted=c(1, 2, NA), weight=c(1, 2, NA), na.rm=TRUE),
                         metric_rmse(actual=c(1, 2),     predicted=c(1, 2),     weight=c(1, 2),     na.rm=TRUE),
                         label = "Check NAs removed correctly")

})

testthat::test_that("Numeric example",{
  actual <- seq(1, 10, 1)
  predicted <- seq(10, 1, -1)
  weight1 <- rep(1, 10)
  weight2 <- c(seq(1, 5, 1), seq(5, 1, -1))


  testthat::expect_equal(metric_rmse(actual, predicted), 33^0.5)
  testthat::expect_equal(metric_rmse(actual, predicted, weight1), 33^0.5)
  testthat::expect_equal(metric_rmse(actual, predicted, weight2), 4.434711565)


})
