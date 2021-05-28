testthat::context("Testing prep_num_bin (numeric binning function)")


testthat::test_that("Testing numeric binning - Error checking - input",{

  # var_to_band must be numeric
  testthat::expect_error(prep_num_bin())
  testthat::expect_error(prep_num_bin(var_to_band=rep("a",10), n_bins=5))
  testthat::expect_error(prep_num_bin(var_to_band=rep(c("a",1), each=5), n_bins=5))

  # var_to_band must be longer than n_bins
  testthat::expect_error(prep_num_bin(var_to_band=c(1,1), n_bins=5))
})

testthat::test_that("Testing numeric binning - Error checking - weights",{
  # Weight must be positive numeric of same length as var_to_band
  testthat::expect_error(prep_num_bin(var_to_band=1:100, weights = 1))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, weights = 1:99))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, weights = rep("a",100)))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, weights = -1:98))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, weights = NULL))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, weights = NA))
})

testthat::test_that("Testing numeric binning - Error checking - binning param",{

  # n_bins must be positive whole number > 1
  testthat::expect_error(prep_num_bin(var_to_band=1:100, n_bins=NA))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, n_bins=-1))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, n_bins=1))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, n_bins=4.5))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, n_bins=c(10,10)))

  testthat::expect_error(prep_num_bin(var_to_band=1:100, method="bad"))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, use_labels="bad"))

  testthat::expect_error(prep_num_bin(var_to_band=1:100, method="gaussian_weight", mean=-1, sd=0.3))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, method="gaussian_weight", mean=2, sd=0.3))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, method="gaussian_weight", mean=0.5, sd=-1))
  testthat::expect_error(prep_num_bin(var_to_band=1:100, method="gaussian_weight", mean=0.5, sd=2))

  testthat::expect_error(prep_num_bin(var_to_band=1:100, n_bins=5, use_labels = "TRUE"))

})



testthat::test_that("Testing numeric binning",{

  #Even bins
  out_value1 <- prep_num_bin(var_to_band=1:20, n_bins=5)
  testthat::expect_equal(out_value1$bins, rep(1:5, each=4))
  testthat::expect_equal(out_value1$labels, c("(1 - 4]", "(4 - 8]", "(8 - 12]", "(12 - 16]", "(16 - 20]"))
  testthat::expect_equal(out_value1$cut_values, c(-Inf, 4, 8, 12, 16, Inf))

  #Check that cuts match bins
  testthat::expect_equal(out_value1$bins, cut(1:20, out_value1$cut_values, labels=FALSE))
  #Check labels.
  #first and last have -Inf and Inf for cut but values for  prep_num_bin.
  #Cut has "," prep has " - "
  testthat::expect_equal(out_value1$labels %>% {.[2:4]},
                         cut(1:20, out_value1$cut_values) %>% levels() %>% gsub(",", " - ", .)  %>% {.[2:4]})

  # Check labels can be used
  out_value2 <- prep_num_bin(var_to_band=1:20, n_bins=5, use_labels = TRUE)
  testthat::expect_equal(as.character(out_value2$bins), rep(c("(1 - 4]", "(4 - 8]", "(8 - 12]", "(12 - 16]", "(16 - 20]"), each=4))
  testthat::expect_equal(out_value2$labels, c("(1 - 4]", "(4 - 8]", "(8 - 12]", "(12 - 16]", "(16 - 20]"))



  #Even bins - non-interger
  #Check cuts returns the same values as bins
  set.seed(666)
  var <- rnorm(n=1000)
  out_value3 <- prep_num_bin(var_to_band=var, n_bins=5)
  testthat::expect_equal(out_value3$bins, cut(var, out_value3$cut_values, labels=FALSE))



  #Gaussian bins
  out_value3 <- prep_num_bin(var_to_band=1:1000, n_bins=25, method="gaussian_weight")
  # Check each bucket is larger than the one nearer the edge
  for (ii in 1:12){
    testthat::expect_true(table(out_value3$bins)[[ii]] <= table(out_value3$bins)[[ii+1]])
    testthat::expect_true(table(out_value3$bins)[[25 - (ii-1)]] <= table(out_value3$bins)[[25 - ii]])
  }

  out_value4 <- prep_num_bin(var_to_band=1:1000, n_bins=25, method="gaussian_weight", mean=0, sd=0.5)
  # Check each bucket is larger than the one nearer the edge
  for (ii in 1:24){
    testthat::expect_true(table(out_value4$bins)[[ii]] >= table(out_value4$bins)[[ii+1]])
  }

  out_value5 <- prep_num_bin(var_to_band=1:1000, n_bins=25, method="gaussian_weight", mean=1, sd=0.5)
  # Check each bucket is larger than the one nearer the edge
  for (ii in 1:24){
    testthat::expect_true(table(out_value5$bins)[[ii]] <= table(out_value5$bins)[[ii+1]])
  }

  # Check it can deal with NAs
  out_value6 <- prep_num_bin(var_to_band=c(NA,1:1000), n_bins=25, method="gaussian_weight", mean=0, sd=0.5)
  testthat::expect_true(is.na(out_value6$bins[1]))

  #Check identical values aren't split
  out_value7 <- prep_num_bin(var_to_band=rep(1, 100), n_bins=25)
  testthat::expect_equal(out_value7$bins, rep(1, 100))

})


testthat::test_that("Testing categorical binning",{

  #This function was not designed for categorical data
  #However it will be used for this data so it should do something sensible and consistent

  var_to_band <- sample(10:20, size=100, replace = TRUE)

  out_value <- prep_num_bin(var_to_band=var_to_band, n_bins=25, method="gaussian_weight", mean=0, sd=0.5)
  out_value_labels <- prep_num_bin(var_to_band=var_to_band, n_bins=25, method="gaussian_weight", mean=0, sd=0.5, use_labels = TRUE)

  testthat::expect_equal(out_value_labels$bins %>% as.character(), #factor to string
                         out_value$labels[out_value$bins] %>% as.character() # numeric to string
                         )

  testthat::expect_equal(var_to_band, #factor to string
                         out_value$labels[out_value$bins] # numeric to string
  )

  #Testing cut gives correct value back
  testthat::expect_equal(out_value$bins,
                         cut(var_to_band, breaks = out_value$cut_values, labels = FALSE)
  )

})
