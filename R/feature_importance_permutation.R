#' feature_importance_permutation
#'
#' @description
#' returns which columns are the most important in the fitted model.
#' This is done by permuting the inputs and measuring the deterioration of the metric
#' This implementation is not suitbile for one-hot encoded categorical variables.
#' The permutation importance is defined to be the difference between the baseline metric and metric from permutating the feature column.
#'
#' @param data dataframe - data from which the model can give predictions.
#' For xgboost it must contain only features used in \code{model} in the correct order
#' Often this dataset is the validation data
#' @param model model object - tested examples are lm glm and xgboost
#' @param actual vector[Numeric] - target to be predicted. Must be normalised by exposure
#' @param weight vector[Numeric] - exposure for predictions
#' @param offset_base_margin numeric - Vector of length \code{nrow(data)} contains offset for model.
#' This multiplies each prediction by \code{exp(offset_base_margin)} and is the same argument passed to a xgb.DMatrix object as \code{base_margin}
#' @param metric function - Of type in admr::metric_ - must have arguments actual, predicted, weight
#' @param nrounds integer - Number of times to permute each feature
#' @param seed integer - random seed for permuations
#' @param minimize boolean - if true the metric is being minimised i.e. lower metric score is better, defaults to TRUE
#' @param ... OPTIONAL: - Arguments included but not defined above will be carried through to the metric
#'
#' @return  dataframe with columns
#'           :col_index - position of feature in \code{data}
#'           :feature - name of feature in \code{data}
#'           :importance_mean - importance of feature
#'           :importance_sd - standard deviation of importance, will be NA if nrounds = 1
#'          this data can be used to find the most important features in the model
#' @export
#'
#' @examples
#'
#' input_data <- data.frame(x1=runif(100, 0, 25), x2=runif(100, 0, 25), x3=runif(100, 0, 25)) %>%
#'   mutate(target=x1^2 * 0.01 + x2 + rnorm(n(),sd=5))
#'
#' #LM
#' model_lm <- glm(target ~ poly(x1, 2) + x2, data=input_data)
#'
#' feature_importance_permutation(data=input_data %>% select(-target), model=model_lm, actual=input_data[["target"]])
#'
#' #GLM
#' model_glm <- glm(target ~ poly(x1, 2) + x2 + x3, data=input_data)
#'
#' feature_importance_permutation(data=input_data %>% select(-target), model=model_glm, actual=input_data[["target"]])
#'
#' #GBM
#' model_gbm <- xgboost(data = as.matrix(input_data %>% select(-target)), label=input_data[["target"]], nrounds=20, verbose = 0)
#'
#'
#' feature_importance_permutation(model=model_gbm,
#'                               data=input_data %>% select(-target),
#'                               actual=input_data[["target"]])
#'
#' feature_importance_permutation(model=model_gbm,
#'                               data=input_data %>% select(-target),
#'                               actual=input_data[["target"]],
#'                               offset_base_margin = rnorm(n=nrow(input_data)))
#'
feature_importance_permutation <- function(data,
                                           model,
                                           actual,
                                           weight=rep(1, nrow(data)),
                                           offset_base_margin=rep(1, nrow(data)),
                                           metric=metric_rmse,
                                           nrounds=10,
                                           seed=666,
                                           minimize = TRUE,
                                           ...){

  # checks on inputs
  checkmate::assert_data_frame(data)
  checkmate::assert_numeric(actual, len = nrow(data))
  checkmate::assert_numeric(weight, len = nrow(data), lower=0)
  checkmate::assert_integerish(nrounds, len=1, lower=1)
  checkmate::assert_integerish(seed, len=1, lower=1)
  checkmate::assert_logical(minimize, len=1)
  kwargs=list(...)
  checkmate::assert_function(metric, args=c("actual", "predicted", "weight", names(kwargs)))
  checkmate::assert_numeric(offset_base_margin, len=nrow(data))


  # extract feature names
  if (!is.null(model$feature_names)){
    model.features <- model$feature_names
  }else{
    model.features <- colnames(data)
  }



  # Define function to get the metric
  get_metric <- function(data_){


    # Get average prediction
    if(any(class(model)=="xgb.Booster")){

      # create dmatrix for standard data
      d_mat <- xgboost::xgb.DMatrix(data=data_  %>%
                                      select(one_of(model$feature_names)) %>%
                                      data.matrix()
                                    , base_margin = offset_base_margin)
      predicted <- predict(object=model, newdata=d_mat) %>% as.vector()
    }else{
      predicted <- predict(object=model, newdata=data_) %>% as.vector() * exp(offset_base_margin)
    }

    score <- metric %>% do.call(append(list(actual=actual, predicted=predicted, weight=weight),
                                       kwargs))
  }

  score_standard <- get_metric(data_=data)

  # Permute features and score -----------------

  permuted_data <- data
  pb <- txtProgressBar(0, length(model.features), style = 3)
  for (ii in 1:length(model.features)){ # for all features
    setTxtProgressBar(pb, ii)

    var_ii <- model.features[ii]
    original_values_ii <- data[[var_ii]] # save original ordering for feature
    scores_permuted_ii <- c() # empty vector to contain scores

    for (kk in 1:nrounds){

      set.seed((((seed+length(model.features)) * ii) + kk)) # set seed

      permuted_data[[var_ii]] <- sample(original_values_ii) # permute feature

      scores_permuted_ii[kk] <- get_metric(data_=permuted_data) # append to vector of scores
    }

    permuted_data[[var_ii]] <- original_values_ii # replace with original ordering

    # create output for feature var_ii
    out_df_ii <- data.frame(col_index=ii,
                            feature=var_ii,
                            importance_mean=score_standard - (scores_permuted_ii %>% mean()),
                            importance_sd=scores_permuted_ii %>% sd(),
                            stringsAsFactors = FALSE)

    # append to existing output

    if (ii==1){
      out_df <- out_df_ii
    }else{
      out_df <- rbind(out_df, out_df_ii)
    }
    gc()
  }

  # If a low score is good switch the ordering
  if (isTRUE(minimize)){
    out_df[["importance_mean"]] = -out_df[["importance_mean"]]
  }

  return(out_df %>% dplyr::arrange(desc(importance_mean), feature))

}
