testthat::context("Testing metrics - Proportion of variance explained")

testthat::test_that("Testing metric properties", {

  constant <- rep(10, 10)
  v1 <- rnorm(n = 10, mean = 10, sd = 1)
  v2 <- rnorm(n = 10, mean = 10, sd = 1)
  weight1 <- rep(1, 10)
  weight2 <- pmax(0, rnorm(n = 10, mean = 10, sd = 1))


  # Check metric is self is 0
  testthat::expect_true(is.na(metric_pove(constant, constant, weight1)), label = "No variance in actuals gives NA") # No variance in target
  testthat::expect_equal(metric_pove(v1, v1, weight2) , 1, label = "Check all variance explained") # All variance explained
  testthat::expect_equal(metric_pove(v1, constant, weight2) , 0, label = "Check no variance explained") # None variance explained

  # Check weights matter
  testthat::expect_false(isTRUE(all.equal(metric_pove(v2, v1, weight1) ,metric_pove(v2, v1, weight2))), label = "weight matters")

  # Check rebasing doesn't matters
  testthat::expect_equal(metric_pove(actual=v1, predicted=v2, weight=weight1), metric_pove(actual=v1, predicted=v2+10, weight=weight1))

})



testthat::test_that("Test errors when input is invalid - lenghts",{
  testthat::expect_error(metric_pove(1:10, 1:9))
  testthat::expect_error(metric_pove(1:10, 1:10, 1:9))
})

testthat::test_that("Test errors when input is invalid - actuals",{
  testthat::expect_error(metric_pove(actual=NA, predicted=c(1, 2)))
  testthat::expect_error(metric_pove(actual=NULL, predicted=c(1, 2)))
  testthat::expect_error(metric_pove(actual=c("a", "b"), predicted=c(1, 2)))
})

testthat::test_that("Test errors when input is invalid - predicted",{
  testthat::expect_error(metric_pove(actual=c(1, 2), predicted=NA))
  testthat::expect_error(metric_pove(actual=c(1, 2), predicted=NULL))
  testthat::expect_error(metric_pove(actual=c(1, 2), predicted=c("a", "b")))
})

testthat::test_that("Test errors when input is invalid - weight",{
  testthat::expect_error(metric_pove(actual=c(1, 2), predicted=c(1, 2), weight=NA))
  testthat::expect_error(metric_pove(actual=c(1, 2), predicted=c(1, 2), weight=c("a", "b")))
  testthat::expect_error(metric_pove(actual=c(1, 2), predicted=c(1, 2), weight=c(-0.1, 1)))
  testthat::expect_error(metric_pove(actual=c(1, 2), predicted=c(1, 2), weight=c(0, 0)))
  testthat::expect_error(metric_pove(actual=c(1, 2), predicted=c(1, 2), weight=NULL))
})

testthat::test_that("Test errors when input is invalid - Other",{

  # na.rm and rebase must be logical
  testthat::expect_error(metric_pove(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), na.rm="True"))
  testthat::expect_error(metric_pove(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), rebase="True"))

  # NA inputs
  testthat::expect_true(is.na(metric_pove(actual=c(1, 2, NA), predicted=c(1, 2, 3))))
  testthat::expect_true(!is.na(metric_pove(actual=c(1 ,2, NA), predicted=c(1, 2, 3), na.rm=TRUE)))

  testthat::expect_equal(metric_pove(actual=c(1, 2, NA), predicted=c(1, 2, NA), weight=c(1, 2, NA), na.rm=TRUE),
                         metric_pove(actual=c(1, 2),     predicted=c(1, 2),     weight=c(1, 2),     na.rm=TRUE),
                         label = "Check NAs removed correctly")

})

testthat::test_that("Numeric example",{
  actual <- seq(2, 20, 2)
  predicted <- seq(1, 10, 1)
  weight1 <- rep(1, 10)
  weight2 <- c(seq(1, 5, 1), seq(5, 1, -1))
  weight3 <- pmax(0, rnorm(n = 10, mean = 10, sd = 1))

  testthat::expect_equal(metric_pove(actual, predicted), 1- (var(actual-predicted)/var(actual)))
  testthat::expect_equal(metric_pove(actual, predicted, weight1), 1- (var(actual-predicted)/var(actual)))
  testthat::expect_equal(metric_pove(actual, predicted, weight2), 1- (var(actual-predicted)/var(actual)))
  testthat::expect_equal(metric_pove(actual, predicted, weight3), 1- (var(actual-predicted)/var(actual))) # Weight cancels out


  set.seed(666)
  predicted <- rnorm(10000, mean=0, sd=10)
  actual <- predicted + rnorm(10000, mean=0, sd=1)
  testthat::expect_equal(metric_pove(actual, predicted), 0.99, tolerance=0.001)

  # https://scikit-learn.org/stable/modules/model_evaluation.html#regression-metrics
  testthat::expect_equal(metric_pove(c(1, 2, 3), c(0.4, 1.5, 3.7)) ,0.4766667, tolerance = .00001, label = "sklearn example")


})

testthat::test_that("logical numeric example",{
  actual <- rnorm(100)
  predicted <- actual * 0.5 # Will have 0.25 of the variance
  weight1 <- pmax(0.1, rnorm(100, mean=10))


  testthat::expect_equal(metric_pove(actual, predicted), 0.75)
  testthat::expect_equal(metric_pove(actual, predicted, weight1), 0.75)



})
