

testthat::context("Testing expl_categorical")

df <- data.frame(pet = rep(c('dog', 'cat', 'horse', 'hamster'), c(40,30,20,10)),
                 age = rep(1:10,10),
                 sex = as.factor(rep(c('M','F', NA), c(50,30,20))),
                 height = as.factor(rep(c('tall', 'medium', 'small', 'tiny' , NA), c(30,20,20,20,10))),
                 weight = rep(c(1.2, 1.3, NA), c(30, 40, 30)),
                 colour = rep(c('brown', 'black', NA), c(40,30,30)),
                 stringsAsFactors = FALSE)

testthat::test_that("check valid input of df: data frame", {
  testthat::expect_error(expl_categorical())
  testthat::expect_error(expl_categorical(fictional))
  testthat::expect_error(expl_categorical('df'))
  testthat::expect_error(expl_categorical(as.matrix(df)))
  testthat::expect_error(expl_categorical(NA))
})

testthat::test_that("check char.level is correct format: positive integer", {
  testthat::expect_error(expl_categorical(df, char.level = -1))
  testthat::expect_error(names(expl_categorical(df, char.level = '20')))
  testthat::expect_error(names(expl_categorical(df, char.level = c(20,15))))
  testthat::expect_error(names(expl_categorical(df, char.level = 1.1)))
  testthat::expect_error(names(expl_categorical(df, char.level = NA)))
})

testthat::test_that("check num.level is correct format: positive integer", {
  testthat::expect_error(expl_categorical(df, num.level = -1))
  testthat::expect_error(names(expl_categorical(df, num.level = '20')))
  testthat::expect_error(names(expl_categorical(df, num.level = c(20,15))))
  testthat::expect_error(names(expl_categorical(df, num.level = 1.1)))
  testthat::expect_error(names(expl_categorical(df, num.level = NA)))
})

testthat::test_that("check passing it 1 column", {

  testthat::expect_warning(
    testthat::expect_equal(expl_categorical(df['weight'], num.level = 2), data.frame(NULL))
  )
  testthat::expect_warning(
    testthat::expect_equal(expl_categorical(df['pet'], char.level = 2), data.frame(NULL))
  )

  expected_output <- data.frame(var = c(rep('pet',4)),
                                level = c('cat', 'dog', 'hamster', 'horse'),
                                class = c(rep('character',4)),
                                n = c(30L, 40L, 10L, 20L),
                                perc = c(30, 40, 10, 20),
                                stringsAsFactors = FALSE)

  testthat::expect_equal(expl_categorical(df['pet'], char.level = 20), expected_output)

  expected_output <- data.frame(var = c(rep('weight',3)),
                                level = c(1.2, 1.3, NA),
                                class = c(rep('numeric',3)),
                                n = c(30, 40, 30),
                                perc = c(30, 40, 30),
                                stringsAsFactors = FALSE)

  testthat::expect_equal(expl_categorical(df['weight'], num.level = 20), expected_output)
})

testthat::test_that("check output by varying char.level", {

  expected_output <- data.frame(var = c(rep('colour',3), rep('sex',3)),
                                level = c('black', 'brown', NA, 'F', 'M', NA),
                                class = c(rep('character',3), rep('factor',3)),
                                n = c(30L, 40L, 30L, 30L, 50L, 20L),
                                perc = c(30, 40, 30, 30, 50, 20),
                                stringsAsFactors = FALSE)

  testthat::expect_equal(expl_categorical(df, char.level = 3, num.level = 0), expected_output)

  expected_output <- data.frame(var = c(rep('colour',3), rep('height', 5), rep('pet',4), rep('sex',3)),
                                level = c('black', 'brown', NA,
                                          'medium' , 'small' , 'tall' , 'tiny' , NA ,
                                          'cat' , 'dog' , 'hamster' , 'horse' ,
                                          'F' , 'M' , NA),
                                class = c(rep('character',3), rep('factor',5), rep('character',4), rep('factor',3)),
                                n = c(30, 40, 30, 20, 20, 30, 20, 10, 30, 40, 10, 20, 30, 50, 20),
                                perc = c(30, 40, 30, 20, 20, 30, 20, 10, 30, 40, 10, 20, 30, 50, 20),
                                stringsAsFactors = FALSE)

  testthat::expect_equal(expl_categorical(df, char.level = 20, num.level = 0), expected_output)
  testthat::expect_equal(expl_categorical(df, num.level = 0), expected_output)
})

testthat::test_that("check output by varying num.level", {

  expected_output <- data.frame(var = rep('weight',3),
                                level = c(1.2, 1.3, NA),
                                class = c(rep('numeric',3)),
                                n = c(30, 40, 30),
                                perc = c(30, 40, 30),
                                stringsAsFactors = FALSE)

  testthat::expect_equal(expl_categorical(df, char.level = 0, num.level = 3), expected_output)

  expected_output <- data.frame(var = c(rep('age',10), rep('weight',3)),
                                level = c(1L:10L, 1.2, 1.3, NA),
                                class = c(rep('integer',10), rep('numeric',3)),
                                n = c(rep(10,10), 30, 40, 30),
                                perc = c(rep(10,10), 30, 40, 30),
                                stringsAsFactors = FALSE)

  testthat::expect_equal(expl_categorical(df, char.level = 0, num.level = 11), expected_output)

  expected_output <- data.frame(var = c(rep('age',10), rep('weight',3)),
                                level = c(1:10, 1.2, 1.3, NA),
                                class = c(rep('integer',10), rep('numeric',3)),
                                n = c(rep(10,10), 30, 40, 30),
                                perc = c(rep(10,10), 30, 40, 30),
                                stringsAsFactors = FALSE)

  testthat::expect_equal(expl_categorical(df, char.level = 0), expected_output)

})

testthat::test_that("check output for data.table", {

  # Check it works with data.tables
  # Check it works with factors and ordered factors
  test_df <- data.table::data.table(cat= as.factor(c("a","b")), char= c("a","b"), order=factor(c("a","b"), ordered=TRUE))

  expected_output <- data.frame(var = c(rep('cat',2), rep('char',2), rep('order',2)),
             level = rep(c("a", "b"), 3),
             class = c(rep('factor',2), rep('character',2), rep('ordered',2)),
             n = rep(1,6),
             perc = rep(50,6),
             stringsAsFactors = FALSE)

  testthat::expect_equal(expl_categorical(test_df), expected_output)

})

testthat::test_that("check output for numeric", {

  df <- data.frame(male = rep(c(1,0),c(20,80)), stringsAsFactors = FALSE)

  expected_output <- data.frame(var = rep('male',2),
                                level = c(0, 1),
                                class = rep('numeric',2),
                                n = c(80,20),
                                perc = c(80,20),
                                stringsAsFactors = FALSE)

  testthat::expect_equal(expl_categorical(df), expected_output)

})

