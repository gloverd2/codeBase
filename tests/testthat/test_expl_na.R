

testthat::context("Testing expl_na")

df <- data.frame(pet = rep(c('dog', 'cat', 'missing', NA), c(40,30,20,10)),

                 age = rep(c(1:9,NA),10),
                 weight = rep(c(1.2, 1.3, 1.4), c(30, 40, 30)),

                 height = as.factor(rep(c('tall', 'medium', 'small', 'missing' , NA), c(30,20,20,20,10))),

                 stringsAsFactors = FALSE)

testthat::test_that("check valid input of df: data frame", {
  testthat::expect_error(expl_na())
  testthat::expect_error(expl_na(NULL))
  testthat::expect_error(expl_na(NA))
  testthat::expect_error(expl_na(fictional))
  testthat::expect_error(expl_na('df'))
  testthat::expect_error(expl_na(as.matrix(df)))
})

testthat::test_that("check na.string is correct format: character string or list", {
  testthat::expect_error(expl_na(df, na.strings = -1))
  testthat::expect_error(expl_na(df, na.strings = 20))
  testthat::expect_error(expl_na(df, na.strings = c(20,15)))
  testthat::expect_error(expl_na(df, na.strings = 1.1))
})

testthat::test_that("check ignore.case is correct format: logical TRUE/FALSE", {
  testthat::expect_error(expl_na(df, ignore.case = -1))
  testthat::expect_error(names(expl_na(df, ignore.case = 'TRUE')))
  testthat::expect_error(names(expl_na(df, ignore.case = c(20,15))))
  testthat::expect_error(names(expl_na(df, ignore.case = 1.1)))
})

testthat::test_that("check output is df", {
  testthat::expect_equal(class(expl_na(df)), "data.frame")
  testthat::expect_equal(class(expl_na(df, na.strings = 'non-missing')), "data.frame")
  testthat::expect_equal(class(expl_na(df, na.strings = 'missing')), "data.frame")
})

testthat::test_that("check output columns is 3", {
  testthat::expect_equal(ncol(expl_na(df)), 3)
  testthat::expect_equal(ncol(expl_na(df, na.strings = 'non-missing')), 3)
  testthat::expect_equal(ncol(expl_na(df, na.strings = 'missing')), 3)
})

testthat::test_that("check output rows is number of columns in df", {
  testthat::expect_equal(nrow(expl_na(df)), 4)
  testthat::expect_equal(nrow(expl_na(df, na.strings = 'non-missing')), 4)
  testthat::expect_equal(nrow(expl_na(df, na.strings = 'missing')), 4)
})

testthat::test_that("check output by varying na.strings", {

  expected_output <- data.frame(var = c('age', 'height', 'pet',   'weight'),
                                na = c(10, 10, 10, 0),
                                perc = c(10, 10, 10, 0))

  testthat::expect_equal(expl_na(df), expected_output)
  testthat::expect_equal(expl_na(df, na.strings = 'non-missing'), expected_output)

  expected_output <- data.frame(var = c('age', 'height', 'pet',   'weight'),
                                na = c(10, 30, 30, 0),
                                perc = c(10, 30, 30, 0))

  testthat::expect_equal(expl_na(df, na.strings = 'missing'), expected_output)

})

testthat::test_that("check output by varying ignore.case", {

  expected_output <- data.frame(var = c('age', 'height', 'pet',   'weight'),
                                na = c(10, 10, 10, 0),
                                perc = c(10, 10, 10, 0))

  testthat::expect_equal(expl_na(df), expected_output)
  testthat::expect_equal(expl_na(df, na.strings = 'Missing'), expected_output)
  testthat::expect_equal(expl_na(df, na.strings = 'Missing', ignore.case = F), expected_output)

  expected_output <- data.frame(var = c('age', 'height', 'pet',   'weight'),
                                na = c(10, 30, 30, 0),
                                perc = c(10, 30, 30, 0))

  testthat::expect_equal(expl_na(df, na.strings = 'Missing', ignore.case = T), expected_output)

})

testthat::test_that("check passing it 1 column", {

  expected_output <- data.frame(var = c('pet'),
                                na = c(10),
                                perc = c(10))

  testthat::expect_equal(expl_na(df['pet']), expected_output)
  testthat::expect_equal(expl_na(df['pet'], na.strings = 'Missing'), expected_output)

  expected_output <- data.frame(var = c('pet'),
                                na = c(30),
                                perc = c(30))

  testthat::expect_equal(expl_na(df['pet'], na.strings = 'missing'), expected_output)
  testthat::expect_equal(expl_na(df['pet'], na.strings = 'Missing', ignore.case = T), expected_output)

})


