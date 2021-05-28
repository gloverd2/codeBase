testthat::context("Testing metrics - precision")

testthat::test_that("Testing metric properties", {

  v1 <- c(rep(0,5), rep(1,5))
  v2 <- seq(0,1, length.out=10)
  weight1 <- rep(1, 10)
  weight2 <- pmax(0, rnorm(n = 10, mean = 10, sd = 1))


  # Check metric is self is 0
  testthat::expect_equal(metric_precision(v1, v1, weight1) , 1, label = "All predictions should be found")
  testthat::expect_equal(metric_precision(v1, v2, weight2) , 1, label = "All predictions should be found")


})




testthat::test_that("Test errors when input is invalid - lengths",{
  testthat::expect_error(metric_precision(actual=c(0,1,1), predicted=c(1,1)))
  testthat::expect_error(metric_precision(actual=c(0,1,1), predicted=c(1,1,1), weight=c(1,1)))
})

testthat::test_that("Test errors when input is invalid - actual",{
  # Check inputs are numeric
  testthat::expect_error(metric_precision(actual=NULL, predicted=c(0.5, 0.5)))
  testthat::expect_error(metric_precision(actual=NA, predicted=c(0.5, 0.5)))
  testthat::expect_error(metric_precision(actual=c("a", "b"), predicted=c(0.5, 0.5)))
  testthat::expect_error(metric_precision(actual=c(1, -1), predicted=c(0.5, 0.5)))
  testthat::expect_error(metric_precision(actual=c(0.5, 1), predicted=c(0.5, 0.5)))

  testthat::expect_warning(metric_precision(actual=c(1, 1), predicted=c(0, 1)), label="only single target")
})

testthat::test_that("Test errors when input is invalid - predicted",{
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=NA))
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=NULL))
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=c("a", "b")))
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=c(1, 2)))
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=c(1, -1)))
})

testthat::test_that("Test errors when input is invalid - weight",{
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=c(0.5, 0.5), weight=NA))
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=c(0.5, 0.5), weight=c(-1, 1)))
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=c(0.5, 0.5), weight=c("a", "b")))
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=c(0.5, 0.5), weight=NULL))
})


testthat::test_that("Test errors when input is invalid - Other",{
  # threshold must be >0 and <1
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=c(0.5, 0.5), weight=c(1, 1), threshold="True"))
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=c(0.5, 0.5), weight=c(1, 1), threshold=-0.1))
  testthat::expect_error(metric_precision(actual=c(0, 1), predicted=c(0.5, 0.5), weight=c(1, 1), threshold=1.1))

})

testthat::test_that("Test NA input",{
  # NA inputs
  testthat::expect_true(is.na(metric_precision(actual=c(0, 1, NA), predicted=c(1, 1, 1))))
  testthat::expect_true(!is.na(sum(metric_precision(actual=c(0 ,1, NA), predicted=c(1, 1, 1), na.rm=TRUE))))

  testthat::expect_equal(metric_precision(actual=c(0, 1, NA), predicted=c(0, 1, NA), weight=c(1, 2, NA), na.rm=TRUE),
                         metric_precision(actual=c(0, 1),     predicted=c(0, 1),     weight=c(1, 2),     na.rm=TRUE),
                         label = "Check NAs removed correctly")

})


testthat::test_that("Numeric example",{
  actual <- c(rep(0,50), rep(1,50))
  predicted <- seq(0, 1, length.out = 100)
  weight <- pmax(0,rnorm(100, mean=1, sd=0.1))

  testthat::expect_equal(metric_precision(actual, predicted, weight), 1)

  expected.out <- matrix(c(sum(weight[51:100]), sum(weight[26:50]), 0, sum(weight[1:25])), byrow=TRUE, ncol = 2)
  colnames(expected.out) <- c("actual.1", "actual.0")
  rownames(expected.out) <- c("predicted.1", "predicted.0")

  testthat::expect_equal(metric_precision(actual, predicted, weight, threshold=0.25), sum(weight[51:100])/sum(weight[51:100],weight[26:50]))


  expected.out <- matrix(c(sum(weight[76:100]), 0, sum(weight[51:75]), sum(weight[1:50])), byrow=TRUE, ncol = 2)
  colnames(expected.out) <- c("actual.1", "actual.0")
  rownames(expected.out) <- c("predicted.1", "predicted.0")

  testthat::expect_equal(metric_precision(actual, predicted, weight, threshold=0.75), 1)



})
