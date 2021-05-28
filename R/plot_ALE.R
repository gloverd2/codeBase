#' plot_ALE
#'
#' @description
#' Plot a 1D Accumulated Local Effects Plot (ALE)
#'
#' @param data dataframe - Data to use for the predictions. Should have same columns as training data (can be training data)
#' @param model model object - Model to produce ALM from
#' @param explain_col string - This is the factor to explain in the model. The columns must be in \code{data}
#' @param weight numeric - Vector of length \code{nrow(data)} contains weightings, if NULL even weighting is used
#' @param offset_base_margin numeric - Vector of length \code{nrow(data)} contains offset for model.
#' This multiples each prediction by \code{exp(offset_base_margin)} and is the same argument passed to a xgb.DMatrix object as \code{base_margin}
#' @param exposure_type -  character. either \code{'pdf'} or \code{'count'}. Method used to plot exposure
#' @param n_bins numeric - This is the number of points to calculate the PDP for
#' @param use_plotly Optional: boolean - If TRUE plotly object is returned else ggplot2 object
#'
#' @seealso plot_PDP
#'
#' @return plotly/ggplot object of ALE plot
#' @export
#'
#' @examples
#'
#' data <- data.frame(x1=runif(100, 0, 25), x2=runif(100, 0, 25)) %>%
#'   mutate(target=x1^2 * 0.01 + x2 + rnorm(n(),sd=5))
#'
#' #LM
#' model_lm <- glm(target ~ poly(x1, 2) + x2, data=data)
#'
#' plot_ALE(data, model_lm, explain_col="x1", n_bins=5)
#' plot_ALE(data, model_lm, explain_col="x2", n_bins=5)
#' #plot_ALE(data, model_lm, explain_col=c("x1","x2"), n_bins=5)
#'
#' #GLM
#' model_glm <- glm(target ~ poly(x1, 2) + x2, data=data)
#'
#' plot_ALE(data, model_glm, explain_col="x1", n_bins=5)
#' plot_ALE(data, model_glm, explain_col="x2", n_bins=5)
#' #plot_ALE(data, model_glm, explain_col=c("x1","x2"), n_bins=5)
#'
#' #GBM
#' model_gbm <- xgboost(data = as.matrix(data[,which(!(names(data)=="target"))]), label=data[["target"]], nrounds=20, verbose = 0)
#' plot_ALE(data[,which(!(names(data)=="target"))], model_gbm, explain_col="x1", n_bins=10)
#' plot_ALE(data[,which(!(names(data)=="target"))], model_gbm, explain_col="x2", n_bins=10)
#' #plot_ALE(data[,which(!(names(data)=="target"))], model_gbm, explain_col=c("x1","x2"), n_bins=10)
#'
plot_ALE <- function(data, model, explain_col, weight=rep(1, nrow(data)), offset_base_margin=rep(0, nrow(data)), exposure_type="pdf", n_bins=10, use_plotly=TRUE){

  #Check inputs
  checkmate::assert_data_frame(data)
  checkmate::assert_character(explain_col, min.len = 1, max.len = 2)
  checkmate::assert_true(all(explain_col %in% names(data)))
  checkmate::assert_integerish(n_bins, len = 1)
  checkmate::assert_logical(use_plotly, len=1)
  checkmate::assert_numeric(weight, len=nrow(data), lower=0)
  checkmate::assert_numeric(offset_base_margin, len=nrow(data))
  checkmate::assert_choice(exposure_type, c("count", "pdf"))


  plot.data <- plotting_numerical_buckets(var_to_band=data[[explain_col]], n_bins=n_bins, weight=weight, include_outliers=TRUE)


  # For each bin
  for (ii in 1:nrow(plot.data)){

    if (ii!=nrow(plot.data)){
      weight_ii <- weight[data[[explain_col]] >= plot.data[[ii,"lower"]] & data[[explain_col]] < plot.data[[ii,"upper"]]]
      offset_base_margin_ii <- offset_base_margin[data[[explain_col]] >= plot.data[[ii,"lower"]] & data[[explain_col]] < plot.data[[ii,"upper"]]]
      data_ii_lower <- data %>% dplyr::filter(.data[[explain_col]] >= plot.data[[ii,"lower"]] & .data[[explain_col]] < plot.data[[ii,"upper"]])
    }else{
      weight_ii <- weight[data[[explain_col]] >= plot.data[[ii,"lower"]] & data[[explain_col]] <= plot.data[[ii,"upper"]]]
      offset_base_margin_ii <- offset_base_margin[data[[explain_col]] >= plot.data[[ii,"lower"]] & data[[explain_col]] <= plot.data[[ii,"upper"]]]
      data_ii_lower <- data %>% dplyr::filter(.data[[explain_col]] >= plot.data[[ii,"lower"]] & .data[[explain_col]] <= plot.data[[ii,"upper"]])
    }


    if (sum(weight_ii) == 0){
      plot.data[ii,"gradient_pred"]=0
      plot.data[ii,"weight"]=0
    }else{


      data_ii_upper <- data_ii_lower

      data_ii_lower[[explain_col]] <- plot.data[[ii,"lower"]] # Set the factor to the bucket min
      data_ii_upper[[explain_col]] <- plot.data[[ii,"upper"]] # Set the factor to the bucket max

      # Get average prediction
      if(any(class(model)=="xgb.Booster")){

        dmat <- xgboost::xgb.DMatrix(data = data_ii_lower %>%
                                        select(one_of(model$feature_names)) %>%
                                        sapply(as.numeric) %>%
                                        matrix(ncol = length(model$feature_names)),
                                      missing = NA,
                                      weight = weight_ii,
                                      base_margin = offset_base_margin_ii)
        plot.data[ii,"lower_pred"] <- sum(predict(object=model, newdata=dmat) %>% as.vector() * weight_ii) / sum(weight_ii)

        dmat <- xgboost::xgb.DMatrix(data = data_ii_upper %>%
                                        select(one_of(model$feature_names)) %>%
                                        sapply(as.numeric) %>%
                                        matrix(ncol = length(model$feature_names)),
                                      missing = NA,
                                      weight = weight_ii,
                                      base_margin = offset_base_margin_ii)
        plot.data[ii,"upper_pred"] <- sum(predict(object=model, newdata=dmat) %>% as.vector() * weight_ii) / sum(weight_ii)
      }else{

        if (any(is.null(offset_base_margin))){ # No offset
          plot.data[ii,"lower_pred"] <- sum(predict(object=model, newdata=data_ii_lower) %>% as.vector() * weight_ii) / sum(weight_ii)
          plot.data[ii,"upper_pred"] <- sum(predict(object=model, newdata=data_ii_upper) %>% as.vector() * weight_ii) / sum(weight_ii)
        }else{ # With offset
          plot.data[ii,"lower_pred"] <- sum(predict(object=model, newdata=data_ii_lower) %>% as.vector() * weight_ii * exp(offset_base_margin_ii)) / sum(weight_ii)
          plot.data[ii,"upper_pred"] <- sum(predict(object=model, newdata=data_ii_upper) %>% as.vector() * weight_ii * exp(offset_base_margin_ii)) / sum(weight_ii)
        }
      }
      plot.data[ii,"gradient_pred"] <- (plot.data[ii,"upper_pred"] - plot.data[ii,"lower_pred"])

      plot.data[ii,"weight"] <- sum(weight_ii)
    }
  }

  plot.data <- plot.data %>%
    dplyr::arrange(lower)

  boundaries <- data.frame(boundaries = c(min(plot.data$lower), plot.data$upper),
                           gradient_pred = c(0, plot.data$gradient_pred),
                           weight = mean(c(NA, plot.data$weight) + c(plot.data$weight, NA), na.rm = TRUE)) %>%
    dplyr::mutate(ALM = cumsum(gradient_pred),
                  ALM0 = ALM - (sum(ALM * weight)/sum(weight)))

  # Normalize by width if required
  if (exposure_type == "pdf"){
    plot.data[["weight"]] <- plot.data[["weight"]] / plot.data[["width"]]
  }

  if (use_plotly==TRUE){

    plotly::plot_ly() %>%
      plotly::add_trace(data=boundaries, y=~ALM0, x=~boundaries, type="scatter", mode="lines+markers", name="Accumulated Local Effect") %>%
      plotly::add_trace(data=plot.data, y=~weight, x=~center, width=~width, type="bar", name="Exposure", yaxis="y2", opacity=0.2) %>%
      plotly::layout(
        title = paste0("ALM for factor ", explain_col),
        yaxis = list(title="Prediction (Accumulated Local Effect)"),
        yaxis2 = list(overlaying = "y", side = "right", title=glue::glue("Exposure ({exposure_type})"), showgrid = FALSE, rangemode="nonnegative"),
        xaxis = list(title=explain_col)
      ) %>%
      return()
  }else{
    scale = (max(boundaries$ALM0) - min(boundaries$ALM0)) / max(plot.data$weight)
    shift = min(boundaries$ALM0)

    out_plot <- ggplot2::ggplot() +
      ggplot2::geom_line(data=boundaries, ggplot2::aes(y=ALM0, x=boundaries, color="Accumulated Local Effect")) +
      ggplot2::geom_point(data=boundaries, ggplot2::aes(y=ALM0, x=boundaries, color="Accumulated Local Effect")) +
      ggplot2::geom_rect(data=plot.data, ggplot2::aes(ymax=(weight * scale) + shift, ymin=shift, xmin=center-(width/2), xmax=center+(width/2), fill="Exposure"), alpha=0.2) +
      ggplot2::scale_y_continuous(name="Prediction (Accumulated Local Effect)", sec.axis=ggplot2::sec_axis(~(. - shift) / scale   ,name=glue::glue("Exposure ({exposure_type})"))) +
      ggplot2::labs(title = paste0("ALM for factor ", explain_col), x=explain_col)

    return(out_plot)
  }
}
