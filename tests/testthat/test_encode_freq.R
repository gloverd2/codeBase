testthat::test_that("Testing categorical frequency encoding",{


  # Single vector input -----------------------------------------------------

  # Simple test
  data_in <- c(rep("cat", 2) , rep("dog", 3), rep("fish", 4), "llama", NA)
  data_out <- encode_freq(data_in)

  testthat::expect_equal(data_out$data, c(rep(4, 2) , rep(3, 3), rep(2, 4), 5, 1))
  testthat::expect_equal(data_out$levels, c("Unknown", "fish", "dog", "cat", "llama", "Other"))

  # Testing level capping
  data_out <- encode_freq(data_in, n_levels=2) # cat and llama should be "other"

  testthat::expect_equal(data_out$data, c(rep(4, 2) , rep(3, 3), rep(2, 4), 4, 1))
  testthat::expect_equal(data_out$levels, c("Unknown", "fish", "dog", "Other"))

  # Testing level capping using the min count
  data_out <- encode_freq(data_in, min_level_count=2) # llama should be "other"

  testthat::expect_equal(data_out$data, c(rep(4, 2) , rep(3, 3), rep(2, 4), 5, 1))
  testthat::expect_equal(data_out$levels, c("Unknown", "fish", "dog", "cat", "Other"))

  # Testing unknowns capping
  data_out <- encode_freq(data_in, unknown_levels="llama") # llama should be "Unknown"

  testthat::expect_equal(data_out$data, c(rep(4, 2) , rep(3, 3), rep(2, 4), 1, 1))
  testthat::expect_equal(data_out$levels, c("Unknown", "fish", "dog", "cat", "Other"))


  # dataframe input ---------------------------------------------------------

  data_in_df <- data.frame(
    pet=c(rep("cat", 2) , rep("dog", 3), rep("fish", 4), "llama", NA),
    letter=c(rep("a",5), rep("b",5), "c")
  )

  data_out <- encode_freq(data=data_in_df)
  testthat::expect_equal(data_out$data[["pet"]], c(rep(4, 2) , rep(3, 3), rep(2, 4), 5, 1))
  testthat::expect_equal(data_out$levels[["pet"]], c("Unknown", "fish", "dog", "cat", "llama", "Other"))
  testthat::expect_equal(data_out$data[["letter"]], c(rep(2, 5) , rep(3, 5), 4))
  testthat::expect_equal(data_out$levels[["letter"]], c("Unknown", "a", "b", "c", "Other"))

  data_out <- encode_freq(data=data_in_df, n_levels=list("pet"=2, "letter"=50), min_level_count=list("pet"=NULL, "letter"=2))
  testthat::expect_equal(data_out$data[["pet"]], c(rep(4, 2) , rep(3, 3), rep(2, 4), 4, 1))
  testthat::expect_equal(data_out$levels[["pet"]], c("Unknown", "fish", "dog", "Other"))
  testthat::expect_equal(data_out$data[["letter"]], c(rep(2, 5) , rep(3, 5), 4))
  testthat::expect_equal(data_out$levels[["letter"]], c("Unknown", "a", "b", "Other"))

  data_out <- encode_freq(data=data_in_df, unknown_levels=list("pet"=c("llama"), "letter"=c("c")))
  testthat::expect_equal(data_out$data[["pet"]], c(rep(4, 2) , rep(3, 3), rep(2, 4), 1, 1))
  testthat::expect_equal(data_out$levels[["pet"]], c("Unknown", "fish", "dog", "cat", "Other"))
  testthat::expect_equal(data_out$data[["letter"]], c(rep(2, 5) , rep(3, 5), 1))
  testthat::expect_equal(data_out$levels[["letter"]], c("Unknown", "a", "b", "Other"))

})
