testthat::context("Testing expl_summary")

set.seed(123)
df <- data.frame(score_raw = rnorm(10, n = 100),
                 dataset = factor(rep(c('VAL', 'TRAIN','TEST'), c(20, 70, 10)),
                                  levels = c('TRAIN', 'VAL', 'TEST')),
                 target = rep(c(1, 0), c(80,20)),
                 stringsAsFactors = FALSE)

df$score10 <- cut(df$score_raw,
                  breaks = quantile(df$score_raw, seq(0,1,0.1)),
                  labels = 1:10,
                  include.lowest = TRUE)

testthat::test_that("check valid input of df: data frame or data table", {
  testthat::expect_error(expl_summary())
  testthat::expect_error(expl_summary(NULL))
  testthat::expect_error(expl_summary(NA))
  testthat::expect_error(expl_summary('df'))
  testthat::expect_error(expl_summary(as.matrix(df)))
  testthat::expect_error(expl_summary(df$score_raw))
  testthat::expect_error(expl_summary(df$dataset))
  testthat::expect_error(expl_summary(list('a', 'b', c(1, 2))))
})

testthat::test_that("check summarise_vars is in df and correct format: character", {
  testthat::expect_error(expl_summary(df, summarise_vars = 5))
  testthat::expect_error(expl_summary(df, summarise_vars = 0.5))
  testthat::expect_error(expl_summary(df, summarise_vars = "scoreraw"))
  testthat::expect_error(expl_summary(df, summarise_vars = NA))
})

testthat::test_that("check group_vars is in df and correct format: character", {
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "datset"))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = NA))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = 5))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = 0.5))
})

testthat::test_that("check remove_missing is correct format: logical TRUE/FALSE", {
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", remove_missing = -1))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", remove_missing = 'TRUE'))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", remove_missing = c(10, 20)))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", remove_missing = 1.5))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", remove_missing = 1))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", remove_missing = factor(TRUE)))
})

testthat::test_that("check row_limit is correct format: numeric", {
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", row_limit = "100"))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", row_limit = -1))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", row_limit = 0.5))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", row_limit = NA))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", row_limit = TRUE))
})

testthat::test_that("check exceeding row_limit gives warning", {
  testthat::expect_warning(testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", row_limit = 1)))
})

testthat::test_that("check order_col_by_stat is correct format: logical TRUE/FALSE", {
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", order_col_by_stat = -1))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", order_col_by_stat = 'TRUE'))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", order_col_by_stat = c(10, 20)))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", order_col_by_stat = 1.5))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", order_col_by_stat = 1))
  testthat::expect_error(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset", order_col_by_stat = factor(TRUE)))
})

testthat::test_that("check output is df", {
  # Written like this to allow tibbles but still check for data.tables.
  testthat::expect(any(class(expl_summary(df, summarise_vars = "score_raw")) == "data.frame") &
                     !any(class(expl_summary(df, summarise_vars = "score_raw")) == "data.table"), failure_message = "Output is either not a df or is a dt.")
  testthat::expect(any(class(expl_summary(df, summarise_vars = c("score_raw", "target"), group_vars = c("dataset", "score10"))) == "data.frame") &
                     !any(class(expl_summary(df, summarise_vars = c("score_raw", "target"), group_vars = c("dataset", "score10"))) == "data.table"), failure_message = "Output is either not a df or is a dt.")
})

testthat::test_that("check output columns", {
  testthat::expect_equal(ncol(expl_summary(df, summarise_vars = "score_raw")), 4)
  testthat::expect_equal(ncol(expl_summary(df, summarise_vars = "score_raw", group_vars = "dataset")), 5)
  testthat::expect_equal(ncol(expl_summary(df, summarise_vars = c("score_raw", "target"), group_vars = c("dataset", "score10"))), 10)
})

testthat::test_that("check output by varying order_col_by_stat", {
  expected_output <- data.frame(score_raw_mean = mean(df$score_raw),
                                score_raw_sd = sd(df$score_raw),
                                score_raw_n = 100,
                                score_raw_na_count = 0,
                                target_mean = mean(df$target),
                                target_sd = sd(df$target),
                                target_n = 100,
                                target_na_count = 0)
  testthat::expect_equal(expl_summary(df, summarise_vars = c("score_raw", "target")), expected_output)
  
  expected_output <- data.frame(score_raw_mean = mean(df$score_raw),
                                target_mean = mean(df$target),
                                score_raw_sd = sd(df$score_raw),
                                target_sd = sd(df$target),
                                score_raw_n = 100,
                                target_n = 100,
                                score_raw_na_count = 0,
                                target_na_count = 0)
  testthat::expect_equal(expl_summary(df, summarise_vars = c("score_raw", "target"), order_col_by_stat = TRUE), expected_output)
})


df2 <- data.frame(score_raw = c(1:8, NA, NA),
                  dataset = factor(rep(c('VAL', 'TRAIN','TEST'), c(4, 4, 2)),
                                   levels = c('TRAIN', 'VAL', 'TEST')),
                  target = rep(c(1, 0), c(8,2)),
                  score10 = c(rep(c(NA, 1, 2), c(2, 5, 3))),
                  stringsAsFactors = FALSE)

testthat::test_that("check output by varying remove_missing", {
  expected_output <- data.frame(mean = 4.5, sd = sd(df2$score_raw, na.rm = TRUE), n = 10, na_count = 2)
  testthat::expect_equal(expl_summary(df2, summarise_vars = "score_raw"), expected_output)
  
  expected_output <- data.frame(mean = as.numeric(NA), sd = as.numeric(NA), n = 10, na_count = 2)
  testthat::expect_equal(expl_summary(df2, summarise_vars = "score_raw", remove_missing = FALSE), expected_output)
  
  expected_output <- data.frame(score10 = c(1, 2, NA), mean = c(1, (1/3), 1), sd = c(0, sd(df2$target[df2$score10 == 2], na.rm = TRUE), 0), n = c(5, 3, 2), na_count = c(0, 0, 0))
  testthat::expect_equal(expl_summary(df2, summarise_vars = "target", group_vars = "score10"), expected_output)
  testthat::expect_equal(expl_summary(df2, summarise_vars = "target", group_vars = "score10", remove_missing = FALSE), expected_output)
})


df3 <- data.frame(score_raw = c(1:10),
                  dataset = factor(rep(c('VAL', 'TRAIN','TEST'), c(4, 4, 2)),
                                   levels = c('TRAIN', 'VAL', 'TEST')),
                  target = rep(c(1, 0), c(8,2)),
                  score10 = c(rep(c(1, 2), c(5, 5))),
                  stringsAsFactors = FALSE)


testthat::test_that("types of input", {
  testthat::expect_error(expl_summary(df3, summarise_vars = NULL, group_vars = "dataset"))
  testthat::expect_error(expl_summary(df3, summarise_vars = NULL, group_vars = c("dataset","score10")))
  
  expected_output <- data.frame(mean = 5.5, sd = sd(df3$score_raw), n = 10, na_count = 0)
  testthat::expect_equal(expl_summary(df3, summarise_vars = "score_raw", group_vars = NULL), expected_output)
  
  expected_output <- data.frame(dataset = factor(c('TRAIN', 'VAL', 'TEST'), levels = c('TRAIN', 'VAL', 'TEST')),
                                mean = c(6.5, 2.5, 9.5),
                                sd = c(sd(df3$score_raw[df3$dataset == 'TRAIN']), sd(df3$score_raw[df3$dataset == 'VAL']), sd(df3$score_raw[df3$dataset == 'TEST'])),
                                n = c(4, 4, 2),
                                na_count = c(0, 0, 0))
  testthat::expect_equal(expl_summary(df3, summarise_vars = "score_raw", group_vars = "dataset"), expected_output)
  
  expected_output <- data.frame(dataset = factor(c('TRAIN', 'TRAIN', 'VAL', 'TEST'), levels = c('TRAIN', 'VAL', 'TEST')),
                                score10 = c(1, 2, 1, 2),
                                mean = c(5.0, 7.0, 2.5, 9.5),
                                sd = c(sd(df3$score_raw[df3$dataset == 'TRAIN' & df3$score10 == 1]), sd(df3$score_raw[df3$dataset == 'TRAIN' & df3$score10 == 2]), sd(df3$score_raw[df3$dataset == 'VAL' & df3$score10 == 1]), sd(df3$score_raw[df3$dataset == 'TEST' & df3$score10 == 2])),
                                n = c(1, 3, 4, 2),
                                na_count = c(0, 0, 0, 0))
  testthat::expect_equal(expl_summary(df3, summarise_vars = "score_raw", group_vars = c("dataset","score10")), expected_output)
  
  expected_output <- data.frame(score_raw_mean = 5.5, score_raw_sd = sd(df3$score_raw), score_raw_n = 10, score_raw_na_count = 0,
                                target_mean = 0.8, target_sd = sd(df3$target), target_n = 10, target_na_count = 0)
  testthat::expect_equal(expl_summary(df3, summarise_vars = c("score_raw","target"), group_vars = NULL), expected_output)
  
  expected_output <- data.frame(dataset = factor(c('TRAIN', 'VAL', 'TEST'), levels = c('TRAIN', 'VAL', 'TEST')),
                                score_raw_mean = c(6.5, 2.5, 9.5),
                                score_raw_sd = c(sd(df3$score_raw[df3$dataset == 'TRAIN']), sd(df3$score_raw[df3$dataset == 'VAL']), sd(df3$score_raw[df3$dataset == 'TEST'])),
                                score_raw_n = c(4, 4, 2),
                                score_raw_na_count = c(0, 0, 0),
                                target_mean = c(1, 1, 0),
                                target_sd = c(0, 0, 0),
                                target_n = c(4, 4, 2),
                                target_na_count = c(0, 0, 0))
  testthat::expect_equal(expl_summary(df3, summarise_vars = c("score_raw","target"), group_vars = "dataset"), expected_output)
  
  expected_output <- data.frame(dataset = factor(c('TRAIN', 'TRAIN', 'VAL', 'TEST'), levels = c('TRAIN', 'VAL', 'TEST')),
                                score10 = c(1, 2, 1, 2),
                                score_raw_mean = c(5.0, 7.0, 2.5, 9.5),
                                score_raw_sd = c(sd(df3$score_raw[df3$dataset == 'TRAIN' & df3$score10 == 1]), sd(df3$score_raw[df3$dataset == 'TRAIN' & df3$score10 == 2]), sd(df3$score_raw[df3$dataset == 'VAL' & df3$score10 == 1]), sd(df3$score_raw[df3$dataset == 'TEST' & df3$score10 == 2])),
                                score_raw_n = c(1, 3, 4, 2),
                                score_raw_na_count = c(0, 0, 0, 0),
                                target_mean = c(1, 1, 1, 0),
                                target_sd = c(NA, 0, 0, 0),
                                target_n = c(1, 3, 4, 2),
                                target_na_count = c(0, 0, 0, 0))
  testthat::expect_equal(expl_summary(df3, summarise_vars = c("score_raw","target"), group_vars = c("dataset","score10")), expected_output)
})


require(data.table)
dt <- as.data.table(df3)

testthat::test_that("testing data.table",{
  testthat::expect_error(expl_summary(dt, summarise_vars = NULL, group_vars = NULL))
  testthat::expect_error(expl_summary(dt, summarise_vars = NULL, group_vars = "dataset"))
  testthat::expect_error(expl_summary(dt, summarise_vars = NULL, group_vars = c("dataset","score10")))
  
  expected_output <- data.table(mean = 5.5, sd = sd(df3$score_raw), n = 10, na_count = 0)
  testthat::expect_equal(expl_summary(dt, summarise_vars = "score_raw", group_vars = NULL), expected_output)
  
  expected_output <- data.table(dataset = factor(c('TRAIN', 'VAL', 'TEST'), levels = c('TRAIN', 'VAL', 'TEST')),
                                mean = c(6.5, 2.5, 9.5),
                                sd = c(sd(df3$score_raw[df3$dataset == 'TRAIN']), sd(df3$score_raw[df3$dataset == 'VAL']), sd(df3$score_raw[df3$dataset == 'TEST'])),
                                n = c(4, 4, 2),
                                na_count = c(0, 0, 0))
  testthat::expect_equal(expl_summary(dt, summarise_vars = "score_raw", group_vars = "dataset"), expected_output)
  
  expected_output <- data.table(dataset = factor(c('TRAIN', 'TRAIN', 'VAL', 'TEST'), levels = c('TRAIN', 'VAL', 'TEST')),
                                score10 = c(1, 2, 1, 2),
                                mean = c(5.0, 7.0, 2.5, 9.5),
                                sd = c(sd(df3$score_raw[df3$dataset == 'TRAIN' & df3$score10 == 1]), sd(df3$score_raw[df3$dataset == 'TRAIN' & df3$score10 == 2]), sd(df3$score_raw[df3$dataset == 'VAL' & df3$score10 == 1]), sd(df3$score_raw[df3$dataset == 'TEST' & df3$score10 == 2])),
                                n = c(1, 3, 4, 2),
                                na_count = c(0, 0, 0, 0))
  testthat::expect_equal(expl_summary(dt, summarise_vars = "score_raw", group_vars = c("dataset","score10")), expected_output)
  
  expected_output <- data.table(score_raw_mean = 5.5, score_raw_sd = sd(df3$score_raw), score_raw_n = 10, score_raw_na_count = 0,
                                target_mean = 0.8, target_sd = sd(df3$target), target_n = 10, target_na_count = 0)
  testthat::expect_equal(expl_summary(dt, summarise_vars = c("score_raw","target"), group_vars = NULL), expected_output)
  
  expected_output <- data.table(dataset = factor(c('TRAIN', 'VAL', 'TEST'), levels = c('TRAIN', 'VAL', 'TEST')),
                                score_raw_mean = c(6.5, 2.5, 9.5),
                                score_raw_sd = c(sd(df3$score_raw[df3$dataset == 'TRAIN']), sd(df3$score_raw[df3$dataset == 'VAL']), sd(df3$score_raw[df3$dataset == 'TEST'])),
                                score_raw_n = c(4, 4, 2),
                                score_raw_na_count = c(0, 0, 0),
                                target_mean = c(1, 1, 0),
                                target_sd = c(0, 0, 0),
                                target_n = c(4, 4, 2),
                                target_na_count = c(0, 0, 0))
  testthat::expect_equal(expl_summary(dt, summarise_vars = c("score_raw","target"), group_vars = "dataset"), expected_output)
  
  expected_output <- data.table(dataset = factor(c('TRAIN', 'TRAIN', 'VAL', 'TEST'), levels = c('TRAIN', 'VAL', 'TEST')),
                                score10 = c(1, 2, 1, 2),
                                score_raw_mean = c(5.0, 7.0, 2.5, 9.5),
                                score_raw_sd = c(sd(df3$score_raw[df3$dataset == 'TRAIN' & df3$score10 == 1]), sd(df3$score_raw[df3$dataset == 'TRAIN' & df3$score10 == 2]), sd(df3$score_raw[df3$dataset == 'VAL' & df3$score10 == 1]), sd(df3$score_raw[df3$dataset == 'TEST' & df3$score10 == 2])),
                                score_raw_n = c(1L, 3L, 4L, 2L),
                                score_raw_na_count = c(0L, 0L, 0L, 0L),
                                target_mean = c(1, 1, 1, 0),
                                target_sd = c(NA, 0, 0, 0),
                                target_n = c(1L, 3L, 4L, 2L),
                                target_na_count = c(0L, 0L, 0L, 0L))
  testthat::expect_equal(expl_summary(dt, summarise_vars = c("score_raw","target"), group_vars = c("dataset","score10")), expected_output)
})

