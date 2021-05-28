testthat::context("Testing feature_importance_permutation")

set.seed(666)
input_data <- data.frame(x1=runif(1000, 0, 25), x2=runif(1000, 0, 25), x3=runif(1000, 0, 25), x4=sample(1:10, size=1000, replace = TRUE)) %>%
  dplyr::mutate(target=x1^2 * 0.01 + x2 + (x4 * 2.5) + rnorm(dplyr::n(),sd=5) + 100)

# LM
model_lm <- glm(target ~ poly(x1, 2) + x2, data=input_data)
# GLM
model_glm <- glm(target ~ poly(x1, 2) + x2, data=input_data)
# GBM
model_gbm <- xgboost::xgboost(data = as.matrix(input_data %>% dplyr::select(-target)), label=input_data[["target"]], nrounds=20, verbose = 0)


testthat::test_that("Testing Data - Should error",{

  testthat::expect_error(feature_importance_permutation(data=NA, model_gbm, data[["target"]]))
  testthat::expect_error(feature_importance_permutation(data="features_df", model_gbm, data[["target"]]))
  testthat::expect_error(feature_importance_permutation(data=1:10, model_gbm, data[["target"]]))
})


testthat::test_that("Testing target - Should error",{

  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_gbm, actual=1:10))
  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_gbm, actual=rep("a", nrow(input_data))))
})


testthat::test_that("Testing weight - Should error",{

  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_gbm, actual=data[["target"]], weight=1:10))
  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_gbm, actual=data[["target"]], rep("a", nrow(input_data))))
})


testthat::test_that("Testing nrounds - Should error",{

  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_gbm, actual=data[["target"]], nrounds=c(1,1)))
  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_gbm, actual=data[["target"]], nrounds="10"))
})


testthat::test_that("Testing seed - Should error",{

  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_gbm, actual=data[["target"]], seed=c(1,1)))
  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_gbm, actual=data[["target"]], seed="10"))
  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_gbm, actual=data[["target"]], seed=10.5))
})



testthat::test_that("Testing the function runs without errors",{

  #Should run with no errors
  #LM
  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_lm, actual=input_data[["target"]]), NA)
  #GLM
  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_glm, actual=input_data[["target"]]), NA)
  #GBM
  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_gbm, actual=input_data[["target"]]), NA)

  #check order
  out <- feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_gbm, actual=input_data[["target"]], nrounds=10)
  testthat::expect_true(all(out %>% dplyr::pull("feature") == c("x2", "x4", "x1", "x3")))

  #check order
  out <- feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_glm, actual=input_data[["target"]], nrounds=10)
  # Features not included

  testthat::expect_true(all(out %>% dplyr::pull("feature") == c("x2", "x1", "x3", "x4")))
  testthat::expect_true(all(out[3:4,] %>% dplyr::pull("importance_mean") == c(0, 0))) # Should be 0 as not fit in the model
  testthat::expect_true(all(out[3:4,] %>% dplyr::pull("importance_sd") == c(0, 0)))


  #Check a different metric
  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target),
                                                        model=model_gbm,
                                                        actual=input_data[["target"]],
                                                        metric=metric_pode,
                                                        family="gaussian")
                         , NA)

  testthat::expect_error(feature_importance_permutation(data=input_data %>% dplyr::select(-target),
                                                        model=model_gbm,
                                                        actual=input_data[["target"]] %>% abs(),
                                                        metric=metric_pode,
                                                        family="poisson")
                         , NA)

})


testthat::test_that("Testing the weight, seed, nrounds and metric matter",{


  out_base <- feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_glm, actual=input_data[["target"]])

  # Same seed and nrounds so should be identical
  out_1 <- feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_glm, actual=input_data[["target"]])
  testthat::expect_identical(out_base, out_1)

  # Different seed should change results
  out_2 <- feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_glm, actual=input_data[["target"]], seed=111)
  testthat::expect_false(isTRUE(all.equal(out_base$importance_mean, out_2$importance_mean)))

  # Different nrounds should change results
  out_3 <- feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_glm, actual=input_data[["target"]], nrounds=25)
  testthat::expect_false(isTRUE(all.equal(out_base$importance_mean, out_3$importance_mean)))

  # Different weights
  out_4 <- feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_glm, actual=input_data[["target"]], weight=runif(nrow(input_data)))
  testthat::expect_false(isTRUE(all.equal(out_base$importance_mean, out_4$importance_mean)))

  # Different metric
  out_5 <- feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_glm, actual=input_data[["target"]], metric=metric_pode, family="gaussian")
  testthat::expect_false(isTRUE(all.equal(out_base$importance_mean, out_5$importance_mean)))

  # Different metric family
  out_6 <- feature_importance_permutation(data=input_data %>% dplyr::select(-target), model=model_glm, actual=input_data[["target"]], metric=metric_pode, family="poisson")
  testthat::expect_false(isTRUE(all.equal(out_5$importance_mean, out_6$importance_mean)))

})

