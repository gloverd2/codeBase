testthat::context("Testing gini coeffient Checking error or no error")

testthat::test_that("Testing actuals and predicted  - Should error",{

  # Check lengths
  testthat::expect_error(metric_gini(actual=1:10, predicted=1:9))

  #Check valid values
  testthat::expect_error(metric_gini(actual=1:10, predicted=NULL))
  testthat::expect_error(metric_gini(actual=1:10, predicted=NA))
  testthat::expect_error(metric_gini(actual=1:10, predicted=rep("a",10)))

  testthat::expect_error(metric_gini(actual=NULL, predicted=1:10))
  testthat::expect_error(metric_gini(actual=NA, predicted=1:10))
  testthat::expect_error(metric_gini(actual=rep("a",10), predicted=1:10))

})


testthat::test_that("Testing weight - Should error",{
  # Check lengths
  testthat::expect_error(metric_gini(actual=1:10, predicted=1:10, weight=1:9))

  # Check valid values
  testthat::expect_error(metric_gini(actual=1:10, predicted=1:10, weight=NA))
  testthat::expect_error(metric_gini(actual=1:10, predicted=1:10, weight=rep("a",10)))
  testthat::expect_error(metric_gini(actual=1:10, predicted=1:10, weight=rep(-1,10)))
  testthat::expect_error(metric_gini(actual=1:10, predicted=1:10, weight=NULL))
})


testthat::test_that("Testing na.rm",{

  #Should error
  testthat::expect_error(metric_gini(actual=1:10, predicted=1:10, na.rm = "TRUE"))
  testthat::expect_error(metric_gini(actual=1:10, predicted=1:10, na.rm = NULL))

  testthat::expect_true(is.na(metric_gini(actual=c(1:10,NA), predicted=c(1:10, NA))))

  act <- sample(c(0,1), size=100, replace = TRUE)
  pred <- runif(n=100)
  rand_weight <- runif(n=100)
  testthat::expect_equal(metric_gini(actual=act, predicted=pred, weight = rand_weight),
                         metric_gini(actual=c(act,1), predicted=c(pred,0.4), weight = c(rand_weight, NA), na.rm=TRUE)
  )

})


testthat::test_that("Testing the function runs without errors",{
  testthat::expect_equal(metric_gini(actual=1:10, predicted=1:10), 1)
  testthat::expect_equal(metric_gini(actual=1:10, predicted=1:10, weight=rep(1,10)), 1)
  testthat::expect_equal(metric_gini(actual=1:10, predicted=seq(10,1,-1)), -1)
  testthat::expect_equal(metric_gini(actual=1:10, predicted=rep(5,10)), 0)


  testthat::expect_equal(metric_gini(actual=1:10, predicted=data.frame(p1=1:10, p2=seq(10,1,-1), p3=rep(5,10)), weight=rep(1,10)), list(p1=1, p2=-1, p3=0))
})


testthat::test_that("Testing weight matters",{

  act <- sample(c(0,1), size=100, replace = TRUE)
  pred <- runif(n=100)
  rand_weight <- runif(n=100)

  testthat::expect_true(metric_gini(actual=act, predicted=pred) != metric_gini(actual=act, predicted=pred, weight=rand_weight))

})
