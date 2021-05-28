testthat::context("Testing check_all_identical - A check for whether a complete list is identical")

testthat::test_that("Testing various lengths of array",{
  # Note: For these tests cases, you must be VERY careful of usage with c().
  # This will make the objects into an array first which can adjust typing.

  testthat::expect_equal(check_all_identical(list(1)),TRUE,label = "Only one element will naturally be identical")
  testthat::expect_equal(check_all_identical(as.list(c(c(NULL,1),c(1)))),TRUE, label = "Works with lists and NULLs in lists")
  testthat::expect_equal(check_all_identical(list(1L,1)), FALSE, label = "Example that passes equality but not identicality")
  testthat::expect_equal(check_all_identical(list(as.character(NA),as.logical(NA))), FALSE, label = "NA example")
  testthat::expect_equal(check_all_identical(as.list(c(1,1,1,1,1,1,2))),FALSE)


  # Error on NULL input
  testthat::expect_error(check_all_identical(NULL), label = "Gives error if only NULL. Note c(NULL,1) is c(1)")

  # These should error due to not being lists.
  testthat::expect_error(check_all_identical(c(1,1,1,1)))
  testthat::expect_error(check_all_identical(0))
  testthat::expect_error(check_all_identical("A"))
  testthat::expect_error(check_all_identical(NA))

  # Examples above working as lists,
  testthat::expect_equal(check_all_identical(as.list(c(1,1,1,1))),TRUE)
  testthat::expect_equal(check_all_identical(list(0,0,0)),TRUE)
  testthat::expect_equal(check_all_identical(list("A","A","A")),TRUE)
  testthat::expect_equal(check_all_identical(list(NA,NA)),TRUE)
  testthat::expect_equal(check_all_identical(list(TRUE,TRUE,TRUE)),TRUE)



  # Examples in documentation:
  testthat::expect_identical(check_all_identical(as.list(c(NA,NA,NA))),TRUE)
  testthat::expect_identical(check_all_identical(list(1,1L,1.00001)),FALSE)
  testthat::expect_identical(check_all_identical(as.list(c("A","A","A","B"))),FALSE)
  testthat::expect_identical(check_all_identical(list(as.double(NA),as.logical(NA))),FALSE)
  testthat::expect_identical(check_all_identical(list(1,1,1,1,1L)),FALSE)
})
