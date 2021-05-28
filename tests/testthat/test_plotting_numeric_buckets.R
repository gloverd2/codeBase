testthat::context("Testing plotting_numerical_buckets")


testthat::test_that("check valid input of df: data frame - should error", {
  testthat::expect_error(plotting_numerical_buckets(rep("a",10)))
  testthat::expect_error(plotting_numerical_buckets(NULL))
  testthat::expect_error(plotting_numerical_buckets(NA))
})


testthat::test_that("check valid input of n_bins - should error", {
  testthat::expect_error(plotting_numerical_buckets(1:100, n_bins=-1))
  testthat::expect_error(plotting_numerical_buckets(1:100, n_bins=1))
  testthat::expect_error(plotting_numerical_buckets(1:100, n_bins=10.5))
  testthat::expect_error(plotting_numerical_buckets(1:100, n_bins="10"))

})


testthat::test_that("check valid input of weight - should error", {
  testthat::expect_error(plotting_numerical_buckets(1:100, n_bins=10, weight=NA))
  testthat::expect_error(plotting_numerical_buckets(1:100, n_bins=10, weight=NULL))
  testthat::expect_error(plotting_numerical_buckets(1:100, n_bins=10, weight=1:99))

})


testthat::test_that("check valid input of include_outliers - should error", {
  testthat::expect_error(plotting_numerical_buckets(1:100, n_bins=10, include_outliers="TRUE"))
  testthat::expect_error(plotting_numerical_buckets(1:100, n_bins=10, include_outliers=c(TRUE,TRUE)))

})


testthat::test_that("Check output - No outliers", {

  out <- plotting_numerical_buckets(0:100, n_bins=10)

  testthat::expect_equal(out$lower, seq(0,90,10))
  testthat::expect_equal(out$upper, seq(10,100,10))
  testthat::expect_equal(out$center, seq(5,95,10))
  testthat::expect_equal(out$width, rep(10,10))


})


testthat::test_that("Check output - With outliers", {

  out_1 <- plotting_numerical_buckets(0:100, n_bins=10)
  out_2 <- plotting_numerical_buckets(c(0:100, 99999), n_bins=10, include_outliers=FALSE)

  testthat::expect_equal(out_2, out_1)

  # check single high outlier
  out_3 <- plotting_numerical_buckets(c(0:100, 99999), n_bins=10, include_outliers=TRUE)

  testthat::expect_equal(out_3$labels[1:10], out_1$labels)
  testthat::expect_equal(out_3[11,]$bin, -1) # Check value is flagged as an outlier
  testthat::expect_equal(out_3[11,]$center, 99999)
  testthat::expect_equal(out_3[11,]$width, 1)

  # check single low outlier #flagged as only negative
  out_4 <- plotting_numerical_buckets(c(0:100, -1), n_bins=10, include_outliers=TRUE)

  testthat::expect_equal(out_4$labels[1:10], out_1$labels)
  testthat::expect_equal(out_4[11,]$bin, -1) # Check value is flagged as an outlier
  testthat::expect_equal(out_4[11,]$center, -1)
  testthat::expect_equal(out_3[11,]$width, 1)

  # check high and low outlier together
  out_5 <- plotting_numerical_buckets(c(0:100, -1, 99999), n_bins=10, include_outliers=TRUE)
  testthat::expect_equal(out_5$labels[1:10], out_1$labels)
  testthat::expect_equal(out_5[11:12,]$bin, c(-1, -2)) # Check values are flagged as an outlier
  testthat::expect_equal(out_5[11:12,]$center, c(-1, 99999))
  testthat::expect_equal(out_5[11:12,]$width, c(1, 1))

  # Check 2 high outliers
  out_6 <- plotting_numerical_buckets(c(0:100, 99999, 99998), n_bins=10, include_outliers=TRUE)
  testthat::expect_equal(out_6$labels[1:10], out_1$labels)
  testthat::expect_equal(out_6[11:12,]$bin, c(-1, -2)) # Check values are flagged as an outlier
  testthat::expect_equal(out_6[11:12,]$center, c(99998, 99999))
  testthat::expect_equal(out_6[11:12,]$width, c(1, 1))

  #Check no flagging for lots of negative values
  testthat::expect_true(all(plotting_numerical_buckets(-100:100, n_bins=10, include_outliers=TRUE)$bin > 0))

  #Check silly negative values are flagged
  testthat::expect_true(!all(plotting_numerical_buckets(c(-100:100, -99999), n_bins=10, include_outliers=TRUE)$bin > 0))

})


testthat::test_that("Check output - With categorical", {

  out <- plotting_numerical_buckets(sample(seq(0,90,10), 100, replace=TRUE), n_bins=10)

  testthat::expect_equal(out$lower, seq(0,90,10) - 0.5 )
  testthat::expect_equal(out$upper, seq(0,90,10) + 0.5)
  testthat::expect_equal(out$center, seq(0,90,10))
  testthat::expect_equal(out$width, rep(1,10))



})


testthat::test_that("Check output - With categorical - tight", {

  out <- plotting_numerical_buckets(sample(seq(0,.90,.10), 100, replace=TRUE), n_bins=10)

  testthat::expect_equal(out$lower, seq(0,.90,.10) - 0.05 )
  testthat::expect_equal(out$upper, seq(0,.90,.10) + 0.05)
  testthat::expect_equal(out$center, seq(0,.90,.10))
  testthat::expect_equal(out$width, rep(.1,10))



})
