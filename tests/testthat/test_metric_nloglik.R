testthat::context("Testing metrics - negative log likelihood")


testthat::test_that("Test errors when input is invalid - length",{
  testthat::expect_error(metric_nloglik(actual=1:10, predicted=1:9))
  testthat::expect_error(metric_nloglik(actual=1:10, predicted=1:10, predicted=1:9))
})

testthat::test_that("Test errors when input is invalid - actuals",{
  testthat::expect_error(metric_nloglik(actual=NA, predicted=c(1, 2)))
  testthat::expect_error(metric_nloglik(actual=NULL, predicted=c(1, 2)))
  testthat::expect_error(metric_nloglik(actual=c("a", "b"), predicted=c(1, 2)))

  #Error check family
  testthat::expect_error(metric_nloglik(actual=c(-1, 2), predicted=c(1, 2), weight=c(1, 1), family="poisson"))
  testthat::expect_error(metric_nloglik(actual=c(-1, 2), predicted=c(1, 2), weight=c(1, 1), family="gamma"))
  testthat::expect_error(metric_nloglik(actual=c(-1, 2), predicted=c(1, 2), weight=c(1, 1), family="gamma"))
  testthat::expect_error(metric_nloglik(actual=c(-1, 2), predicted=c(1, 2), weight=c(1, 1), family="tweedie"))

})

testthat::test_that("Test errors when input is invalid - predicted",{
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=NA))
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=NULL))
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=c("a", "b")))

  #Error check family
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=c(-1, 2), weight=c(1, 1), family="poisson"))
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=c(-1, 2), weight=c(1, 1), family="gamma"))
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=c(-1, 2), weight=c(1, 1), family="tweedie"))
})

testthat::test_that("Test errors when input is invalid - weight",{
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=NA))
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=c("a", "b")))
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=c(-0.1, 1)))
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=c(0, 0)))
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=NULL))
})

testthat::test_that("Test errors when input is invalid - other",{

  # Rebase must be logical
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), rebase="True"))

  # NA inputs
  testthat::expect_true(is.na(metric_nloglik(actual=c(1, 2, NA), predicted=c(1, 2, 3))))
  testthat::expect_true(!is.na(metric_nloglik(actual=c(1 ,2, NA), predicted=c(1, 2, 3), na.rm=TRUE)))

  testthat::expect_equal(metric_nloglik(actual=c(1, 2, NA), predicted=c(1, 2, NA), weight=c(1, 2, NA), na.rm=TRUE),
                         metric_nloglik(actual=c(1, 2),     predicted=c(1, 2),     weight=c(1, 2),     na.rm=TRUE),
                         label = "Check NAs removed correctly")

  #Test family
  testthat::expect_error(metric_nloglik(actual=c(-1, 2), predicted=c(1, 2), weight=c(1, 1), family="llama"))

  #Test tweedie_power
  testthat::expect_error(metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), family="tweedie", tweedie_power=-1))
})


testthat::test_that("Tweedie mappings",{

  testthat::expect_equal(metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), family="tweedie", tweedie_power=0),
                         metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), family="gaussian"))

  testthat::expect_equal(metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), family="tweedie", tweedie_power=1),
                         metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), family="poisson"))

  testthat::expect_equal(metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), family="tweedie", tweedie_power=2),
                         metric_nloglik(actual=c(1, 2), predicted=c(1, 2), weight=c(1, 1), family="gamma"))

})


testthat::test_that("Numeric example",{

  # Work needed here numeric values not yet tested

  # test examples included in function definition

})
