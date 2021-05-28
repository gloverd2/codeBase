testthat::context("Testing plot_feature")

testthat::test_that("Testing examples - Shouldn't error",{

  testthat::expect_error(plot_feature(feature=seq(1,25), target=runif(25)), NA)
  testthat::expect_error(plot_feature(feature=seq(1,50), target=runif(50), split=rep(c("NB","RN"), each=25)), NA)
  testthat::expect_error(plot_feature(feature=seq(1,25), target=runif(25), exposure_type="count"), NA)
  testthat::expect_error(plot_feature(feature=rnorm(100) , target=runif(100)), NA)
  testthat::expect_error(plot_feature(feature=rnorm(100), target=runif(100), exposure_type="count"), NA)
  testthat::expect_error(plot_feature(feature=rnorm(1000), target=runif(1000), split=sample(c("a", "b", "c"), 1000, replace = TRUE, prob=c(0.5, 0.3, 0.2))), NA)
  testthat::expect_error(plot_feature(feature=c(rnorm(900), rep(c(999, NA),50)), target=runif(1000), split=sample(c("a", "b", "c"), 1000, replace = TRUE, prob=c(0.5, 0.3, 0.2))), NA)

  testthat::expect_error(plot_feature(feature=c(rnorm(100), -999,999), target=runif(102), exposure_type="pdf"), NA)
  testthat::expect_error(plot_feature(feature=c(NA, seq(1,24)), target=runif(25), exposure_type="count"), NA)
  testthat::expect_error(plot_feature(feature=c(NA, seq(1,23), 999), target=runif(25), exposure_type="pdf"), NA)
  testthat::expect_error(plot_feature(feature=rep(c("a", "b", "c", "d", "e"), 5), target=runif(25)), NA)
  testthat::expect_error(plot_feature(feature=rep(c("a", "b", "c", "d", NA), 5), target=runif(25)), NA)
  testthat::expect_error(plot_feature(feature=rep(c("a", "b", "c", "d", NA), 5), target=runif(25), split=sample(c("a", "b", "c"), 25, replace = TRUE, prob=c(0.5, 0.3, 0.2))), NA)

})


testthat::test_that("Testing inputs",{

  # Check it can deal with single and pars of values (numeric, character and factor)
  testthat::expect_error(plot_feature(feature=rep(1, 25), target=runif(25)), NA)
  testthat::expect_error(plot_feature(feature=rep(c(1, 2), 25), target=runif(50)), NA)
  testthat::expect_error(plot_feature(feature=rep(c("a", "b"), 25), target=runif(50)), NA)
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50)), NA)

  # Should break when lengths are different
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(51))) # Should break

  # Testing weight
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), weight=rep(1, 50)), NA) # Should run
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), weight=rep(1, 51))) # Shouldn't run
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), weight=c(-1, rep(1, 49))))

  #Testing splits
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), split=rep(c("a", "b"), 25)), NA)
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), split=1:25)) # Too many splits
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), split=rep(c("a", "b"), 24))) # wrong shape

  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), weight=rep(1, 50), feature_name=1))
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), weight=rep(1, 50), feature_name=c("f1", "f2")))

  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), weight=rep(1, 50), n_bins=-1))
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), weight=rep(1, 50), n_bins=c(10,10)))

  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), weight=rep(1, 50), exposure_type="pdf"),NA) # Allowed
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), weight=rep(1, 50), exposure_type="count"),NA) # Allowed
  testthat::expect_error(plot_feature(feature=factor(rep(c("a", "b"), 25)), target=runif(50), weight=rep(1, 50), exposure_type="other")) #Not Allowed
})


