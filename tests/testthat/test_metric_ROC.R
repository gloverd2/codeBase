testthat::context("Testing ROC curve plot. Checking error or no error")

testthat::test_that("Testing actuals and predicted  - Should error",{

  # Check lengths
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,9)))

  #Check valid values
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=NULL))
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=NA))
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep("a",10)))
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=c(rep(-0.1,5),rep(1,5))))
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=c(rep(0,5),rep(1.1,5))))


  testthat::expect_error(metric_ROC(actual=NULL, predicted=rep(0.5,10)))
  testthat::expect_error(metric_ROC(actual=NA, predicted=rep(0.5,10)))
  testthat::expect_error(metric_ROC(actual=rep("a",10), predicted=rep(0.5,10)))
  testthat::expect_error(metric_ROC(actual=rep(-1,10), predicted=rep(0.5,10)))
  testthat::expect_error(metric_ROC(actual=rep(2,10), predicted=rep(0.5,10)))
  testthat::expect_error(metric_ROC(actual=rep(0.5,10), predicted=rep(0.5,10)))

})


testthat::test_that("Testing weight - Should error",{
  # Check lengths
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=1:9))

  # Check valid values
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=NA))
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=rep("a",10)))
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=rep(-1,10)))
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=NULL))
})


testthat::test_that("Testing na.rm",{

  #Should error
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=seq(0,1, length.out = 10), na.rm = "TRUE"))
  testthat::expect_error(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=seq(0,1, length.out = 10), na.rm = NULL))

  testthat::expect_true(is.na(metric_ROC(actual=c(rep(0,5),rep(1,5),NA), predicted=c(seq(0,1, length.out = 10), NA))))

  act <- sample(c(0,1), size=100, replace = TRUE)
  pred <- runif(n=100)
  rand_weight <- runif(n=100)
  testthat::expect_equal(metric_ROC(actual=act, predicted=pred, weight = rand_weight),
                         metric_ROC(actual=c(act,1), predicted=c(pred,0.4), weight = c(rand_weight, NA), na.rm=TRUE)
                         )

})


testthat::test_that("Testing the function runs without errors",{

  testthat::expect_equal(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=seq(0,1, length.out = 10)), 1)
  testthat::expect_equal(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10)), 0.5)

  testthat::expect_equal(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=seq(0,1, length.out = 10), weight=rep(1,10)), 1)
  testthat::expect_equal(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=rep(0.5,10), weight=rep(1,10)), 0.5)

  testthat::expect_equal(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=data.frame(p1=c(rep(0,5),rep(1,5)), p2=rep(0.5,10))), list(p1=1, p2=0.5))
  testthat::expect_equal(metric_ROC(actual=c(rep(0,5),rep(1,5)), predicted=data.frame(p1=c(rep(0,5),rep(1,5)), p2=rep(0.5,10)), weight=rep(1,10)), list(p1=1, p2=0.5))

  #Taken from sklearn example
  testthat::expect_equal(metric_ROC(actual=c(0,0,1,1), predicted=c(0.1, 0.4, 0.35, 0.8)), 0.75)

  # Tested using ROCR
  set.seed(666)
  actual <- c(rep(0, 50), rep(1, 50))
  predicted <- rnorm(n=100, mean=(1:100)/100, sd=0.2) %>% pmax(0) %>% pmin(1)
  testthat::expect_equal(metric_ROC(actual=actual, predicted=predicted), 0.9128)

})


testthat::test_that("Testing weight matters",{

  act <- sample(c(0,1), size=100, replace = TRUE)
  pred <- runif(n=100)
  rand_weight <- runif(n=100)

  testthat::expect_true(metric_ROC(actual=act, predicted=pred) != metric_ROC(actual=act, predicted=pred, weight=rand_weight))

})
