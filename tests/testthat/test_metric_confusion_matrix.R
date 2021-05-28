testthat::context("Testing metrics - Confusion matrix")

testthat::test_that("Testing metric properties", {

  v1 <- c(rep(1,5), rep(0,5))
  v2 <- seq(0,1, length.out=10)
  weight1 <- rep(1, 10)
  weight2 <- pmax(0, rnorm(n = 10, mean = 10, sd = 1))


  # Check metric is self is 0
  testthat::expect_equal(sum(metric_confusion_matrix(v1, v1, weight1)) ,sum(weight1), label = "All predictions should be found")
  testthat::expect_equal(sum(metric_confusion_matrix(v1, v2, weight2)) ,sum(weight2), label = "All predictions should be found")

  # Check threshold matters
  testthat::expect_false(isTRUE(all.equal(metric_confusion_matrix(v1, v2, weight1, threshold=0.3) ,metric_confusion_matrix(v1, v2, weight1, threshold=0.7))), label = "weight matters")


})



testthat::test_that("Test errors when input is invalid - lengths",{

  # Error Catching ----------------------------------------------------------
  # Check lengths of inputs
  testthat::expect_error(metric_confusion_matrix(actual=c(0,1,1), predicted=c(1,1)))
  testthat::expect_error(metric_confusion_matrix(actual=c(0,1,1), predicted=c(0,1,1), weight=c(1,1)))
})

testthat::test_that("Test errors when input is invalid - actual",{
  # Check inputs are numeric
  testthat::expect_error(metric_confusion_matrix(actual=NULL, predicted=c(0.5, 0.5)))
  testthat::expect_error(metric_confusion_matrix(actual=NA, predicted=c(0.5, 0.5)))
  testthat::expect_error(metric_confusion_matrix(actual=c("a", "b"), predicted=c(0.5, 0.5)))
  testthat::expect_error(metric_confusion_matrix(actual=c(1, -1), predicted=c(0.5, 0.5)))
  testthat::expect_error(metric_confusion_matrix(actual=c(0.5, 1), predicted=c(0.5, 0.5)))

  testthat::expect_warning(metric_confusion_matrix(actual=c(1, 1), predicted=c(0, 1)), label="only single target")
})

testthat::test_that("Test errors when input is invalid - predicted",{
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=NA))
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=NULL))
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=c("a", "b")))
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=c(1, 2)))
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=c(1, -1)))
})

testthat::test_that("Test errors when input is invalid - weight",{
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=c(0.5, 0.5), weight=NA))
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=c(0.5, 0.5), weight=c(-1, 1)))
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=c(0.5, 0.5), weight=c("a", "b")))
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=c(0.5, 0.5), weight = NULL))
})


testthat::test_that("Test errors when input is invalid - Other",{
  # threshold must be >0 and <1
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=c(0.5, 0.5), weight=c(1, 1), threshold="True"))
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=c(0.5, 0.5), weight=c(1, 1), threshold=-0.1))
  testthat::expect_error(metric_confusion_matrix(actual=c(1, 0), predicted=c(0.5, 0.5), weight=c(1, 1), threshold=1.1))

})

testthat::test_that("Test NA input",{
  # NA inputs

  NA_out <- metric_confusion_matrix(actual=c(0, 1, NA), predicted=c(1, 1, 1))

  testthat::expect_true(all(is.na(NA_out %>% as.vector())))
  testthat::expect_equal(colnames(NA_out), c("actual.1", "actual.0"))
  testthat::expect_equal(rownames(NA_out), c("predicted.1", "predicted.0"))

  testthat::expect_equal(metric_confusion_matrix(actual=c(0, 1, NA), predicted=c(0, 1, NA), weight=c(1, 2, NA), na.rm=TRUE),
                         metric_confusion_matrix(actual=c(0, 1),     predicted=c(0, 1),     weight=c(1, 2)),
                         label = "Check NAs removed correctly")

  testthat::expect_equal(metric_confusion_matrix(actual=c(0, 1, 1),  predicted=c(0, 1, 1), weight=c(1, 2, NA), na.rm=TRUE),
                         metric_confusion_matrix(actual=c(0, 1),     predicted=c(0, 1),     weight=c(1, 2)),
                         label = "Check NAs removed correctly")

})


testthat::test_that("Numeric example",{
  actual <- c(rep(0,50), rep(1,50))
  predicted <- seq(0, 1, length.out = 100)
  weight <- pmax(0,rnorm(100, mean=1, sd=0.1))

  expected.out <- matrix(c(sum(weight[51:100]), 0, 0, sum(weight[1:50])), byrow=TRUE, ncol = 2)
  colnames(expected.out) <- c("actual.1", "actual.0")
  rownames(expected.out) <- c("predicted.1", "predicted.0")

  testthat::expect_equal(metric_confusion_matrix(actual, predicted, weight), expected.out)

  expected.out <- matrix(c(sum(weight[51:100]), sum(weight[26:50]), 0, sum(weight[1:25])), byrow=TRUE, ncol = 2)
  colnames(expected.out) <- c("actual.1", "actual.0")
  rownames(expected.out) <- c("predicted.1", "predicted.0")

  testthat::expect_equal(metric_confusion_matrix(actual, predicted, weight, threshold=0.25), expected.out)


  expected.out <- matrix(c(sum(weight[76:100]), 0, sum(weight[51:75]), sum(weight[1:50])), byrow=TRUE, ncol = 2)
  colnames(expected.out) <- c("actual.1", "actual.0")
  rownames(expected.out) <- c("predicted.1", "predicted.0")

  testthat::expect_equal(metric_confusion_matrix(actual, predicted, weight, threshold=0.75), expected.out)



})
