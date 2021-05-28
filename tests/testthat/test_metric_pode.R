testthat::context("Testing metrics - Proportion of deviance explained")

testthat::test_that("Testing metric properties", {

  constant <- rep(10, 10)
  v1 <- rnorm(n = 10, mean = 10, sd = 1)
  v2 <- rnorm(n = 10, mean = 10, sd = 1)
  weight1 <- rep(1, 10)
  weight2 <- pmax(0, rnorm(n = 10, mean = 10, sd = 1))


  # Check metric is self is 0
  testthat::expect_warning(metric_pode(constant, constant, weight1), label = "metric with self is 0") # No deviance in target
  testthat::expect_equal(metric_pode(v1, v1, weight2) , 1, label = "Check all deviance explained") # All deviance explained
  testthat::expect_equal(metric_pode(v1, constant, weight2, rebase = TRUE) , 0, label = "Check no deviance explained") # None deviance explained

  # Check weights matter
  testthat::expect_false(isTRUE(all.equal(metric_pode(v2, v1, weight1) ,metric_pode(v2, v1, weight2))), label = "weight matters")

  # Check rebasing matters
  testthat::expect_equal(metric_pode(actual=v1, predicted=v1, weight=weight1), metric_pode(actual=v1, predicted=v1+10, weight=weight1, rebase = TRUE))

})


testthat::test_that("Test errors when input is invalid - length",{
  testthat::expect_error(metric_pode(actual=1:10, predicted=1:9))
  testthat::expect_error(metric_pode(actual=1:10, predicted=1:10, predicted=1:9))
})

testthat::test_that("Test errors when input is invalid - actuals",{
  testthat::expect_error(metric_pode(actual=NA, predicted=c(1, 2)))
  testthat::expect_error(metric_pode(actual=NULL, predicted=c(1, 2)))
  testthat::expect_error(metric_pode(actual=c("a", "b"), predicted=c(1, 2)))

  #Error check family
  testthat::expect_error(metric_pode(actual=c(-1, 2), predicted=c(1, 2), weight=c(1, 1), family="poisson"))
  testthat::expect_error(metric_pode(actual=c(-1, 2), predicted=c(1, 2), weight=c(1, 1), family="gamma"))
  testthat::expect_error(metric_pode(actual=c(-1, 2), predicted=c(1, 2), weight=c(1, 1), family="gamma"))
  testthat::expect_error(metric_pode(actual=c(-1, 2), predicted=c(1, 2), weight=c(1, 1), family="tweedie"))

})

testthat::test_that("Test errors when input is invalid - predicted",{
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=NA))
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=NULL))
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=c("a", "b")))

  #Error check family
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=c(-1, 2), weight=c(1, 1), family="poisson"))
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=c(-1, 2), weight=c(1, 1), family="gamma"))
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=c(-1, 2), weight=c(1, 1), family="tweedie"))
})

testthat::test_that("Test errors when input is invalid - weight",{
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=c(1, 2), weight=NA))
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=c(1, 2), weight=c("a", "b")))
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=c(1, 2), weight=c(-0.1, 1)))
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=c(1, 2), weight=c(0, 0)))
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=c(1, 2), weight=NULL))
})

testthat::test_that("Test errors when input is invalid - other",{

  # Rebase must be logical
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), rebase="True"))

  # NA inputs
  testthat::expect_true(is.na(metric_pode(actual=c(1, 2, NA), predicted=c(1, 2, 3))))
  testthat::expect_true(!is.na(metric_pode(actual=c(1 ,2, NA), predicted=c(1, 2, 3), na.rm=TRUE)))

  testthat::expect_equal(metric_pode(actual=c(1, 2, NA), predicted=c(1, 2, NA), weight=c(1, 2, NA), na.rm=TRUE),
                         metric_pode(actual=c(1, 2),     predicted=c(1, 2),     weight=c(1, 2),     na.rm=TRUE),
                         label = "Check NAs removed correctly")

  #Test family
  testthat::expect_error(metric_pode(actual=c(-1, 2), predicted=c(1, 2), weight=c(1, 1), family="llama"))

  #Test tweedie_power
  testthat::expect_error(metric_pode(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), family="tweedie", tweedie_power=-1))
})

testthat::test_that("Numeric example",{

  # Work needed here numeric values not yet tested

  # test examples included in function definition

})
