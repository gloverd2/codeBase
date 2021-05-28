#' plot_PDP
#'
#' @description
#' Plot a 1D or 2D Partial Dependence Plot (PDP)
#'
#' @param data dataframe - Data to use for the predictions. Should have same columns as training data (can be training data)
#' @param model model object - Model to produce PDP from
#' @param explain_col string - Vector of length 1 for 1D plot or length 2 for 2D plot. This is the factor(s) to explain in the model. The columns must be in \code{data}
#' @param weight numeric - Vector of length \code{nrow(data)} contains weightings, if NULL even weighting is used
#' @param offset_base_margin numeric - Vector of length \code{nrow(data)} contains offset for model.
#' This multiples each prediction by \code{exp(offset_base_margin)} and is the same argument passed to a xgb.DMatrix object as \code{base_margin}
#' @param exposure_type -  character. either \code{'pdf'} or \code{'count'}. Method used to plot exposure
#' @param n_bins numeric - Vector of length 1 for 1D plot and 1 or 2 for 2D plots. This is the number of points to calculate the PDP for
#' @param use_plotly Optional: boolean - If TRUE plotly object is returned else ggplot2 object
#'
#' @seealso plot_ALE
#'
#' @return plotly/ggplot object of PDP plot
#' @export
#'
#' @examples
#'
#' data <- data.frame(x1=runif(100, 0, 25), x2=runif(100, 0, 25)) %>%
#'   dplyr::mutate(target=x1^2 * 0.01 + x2 + rnorm(n(),sd=5))
#'
#' #LM
#' model_lm <- glm(target ~ poly(x1, 2) + x2, data=data)
#'
#' plot_PDP(data, model_lm, explain_col="x1", n_bins=5)
#' plot_PDP(data, model_lm, explain_col="x2", n_bins=5)
#' plot_PDP(data, model_lm, explain_col=c("x1","x2"), n_bins=5)
#'
#' #GLM
#' model_glm <- glm(target ~ poly(x1, 2) + x2, data=data)
#'
#' plot_PDP(data, model_glm, explain_col="x1", n_bins=5)
#' plot_PDP(data, model_glm, explain_col="x2", n_bins=5)
#' plot_PDP(data, model_glm, explain_col=c("x1","x2"), n_bins=5)
#'
#' #GBM
#' model_gbm <- xgboost(data = as.matrix(data[,which(!(names(data)=="target"))]), label=data[["target"]], nrounds=20, verbose = 0)
#' plot_PDP(data[,which(!(names(data)=="target"))], model_gbm, explain_col="x1", n_bins=10)
#' plot_PDP(data[,which(!(names(data)=="target"))], model_gbm, explain_col="x2", n_bins=10)
#' plot_PDP(data[,which(!(names(data)=="target"))], model_gbm, explain_col=c("x1","x2"), n_bins=10)
#'
plot_PDP <- function(data, model, explain_col, weight=rep(1, nrow(data)), offset_base_margin=NULL, exposure_type="pdf", n_bins=10, use_plotly=TRUE){

  #Check inputs
  checkmate::assert_data_frame(data)
  checkmate::assert_character(explain_col, min.len = 1, max.len = 2)
  checkmate::assert_true(all(explain_col %in% names(data)))
  checkmate::assert_integerish(n_bins, min.len = 1, max.len = 2)
  checkmate::assert_logical(use_plotly, len=1)
  checkmate::assert_numeric(weight, len=nrow(data), lower=0)
  checkmate::assert_numeric(offset_base_margin, len=nrow(data), null.ok = TRUE)
  checkmate::assert_choice(exposure_type, c("count", "pdf"))

  if (length(explain_col)==1){ # 1D Plot

    checkmate::assert_integerish(n_bins, len=1)


    plot.data <- plotting_numerical_buckets(var_to_band=data[[explain_col]], n_bins=n_bins, weight=weight, include_outliers=TRUE)

    # For each bin
    for (ii in 1:nrow(plot.data)){

      if(ii==1){
        data_ii <- data
      }
      data_ii[[explain_col]] <- plot.data$center[ii] # Set the factor to the bucket center

      # Get average prediction
      if(any(class(model)=="xgb.Booster")){
        dmat <- xgboost::xgb.DMatrix(data = data_ii %>%
                                        select(one_of(model$feature_names)) %>%
                                        sapply(as.numeric) %>%
                                        matrix(ncol = length(model$feature_names)),
                                      missing = NA,
                                      weight = weight,
                                      base_margin = offset_base_margin)
        plot.data[ii,"mean_pred"] <- sum(predict(object=model, newdata=dmat) %>% as.vector() * weight) / sum(weight)
      }else{

        if (any(is.null(offset_base_margin))){ # No offset
          plot.data[ii,"mean_pred"] <- sum(predict(object=model, newdata=data_ii) %>% as.vector() * weight) / sum(weight)
        }else{ # With offset
          plot.data[ii,"mean_pred"] <- sum(predict(object=model, newdata=data_ii) %>% as.vector() * weight * exp(offset_base_margin)) / sum(weight)
        }

      }

      # Get weight of bucket
      if(ii!=nrow(plot.data)){
        plot.data[ii,"weight"] <- sum(weight[data[[explain_col]] >= plot.data[[ii,"lower"]] & data[[explain_col]] < plot.data[[ii,"upper"]]])
      }
      else{
        plot.data[ii,"weight"] <- sum(weight[data[[explain_col]] >= plot.data[[ii,"lower"]] & data[[explain_col]] <= plot.data[[ii,"upper"]]])
      }

    }

    # Normalize by width if required
    if (exposure_type == "pdf"){
      plot.data[["weight"]] <- plot.data[["weight"]] / plot.data[["width"]]
    }

    # Plot PDP
    if (use_plotly==TRUE){
      plotly::plot_ly(data=plot.data) %>%
        plotly::add_trace(y=~mean_pred, x=~center, type="scatter", mode="lines+markers", name="Partial Dependence") %>%
        plotly::add_trace(y=~weight, x=~center, width=~width, type="bar", name="Exposure", yaxis="y2", opacity=0.2) %>%
        plotly::layout(
          title = paste0("PDP for factor ", explain_col),
          yaxis = list(title="Mean prediction"),
          yaxis2 = list(overlaying = "y", side = "right", title=glue::glue("Exposure ({exposure_type})"), showgrid = FALSE, rangemode="nonnegative"),
          xaxis = list(title=explain_col)
        ) %>%
        return()
    }else{

      scale= mean(plot.data$mean_pred) / mean(plot.data$weight)

      out_plot <- ggplot2::ggplot(data=plot.data) +
        ggplot2::geom_line(ggplot2::aes(y=mean_pred, x=center, color="Partial Dependence")) +
        ggplot2::geom_point(ggplot2::aes(y=mean_pred, x=center, color="Partial Dependence")) +
        ggplot2::geom_rect(ggplot2::aes(ymax=weight  * scale, ymin=0, xmin=center-(width/2), xmax=center+(width/2), fill="Exposure"), alpha=0.2) +
        ggplot2::scale_y_continuous(name="Mean prediction", sec.axis=ggplot2::sec_axis(~./scale ,name=glue::glue("Exposure ({exposure_type})"))) +
        ggplot2::labs(title = paste0("PDP for factor ", explain_col), x=explain_col)

      return(out_plot)
    }


  }else if (length(explain_col)==2){ # 2D Plot

    #Check explain columns aren't the same
    checkmate::assert_true(explain_col[1]!=explain_col[2])
    checkmate::assert_numeric(n_bins, min.len = 1, max.len = 2)

    n_bins.x = ifelse(length(n_bins)==1, n_bins, n_bins[1])
    n_bins.y = ifelse(length(n_bins)==1, n_bins, n_bins[2])


    plot.data.x <- plotting_numerical_buckets(var_to_band=data[[explain_col[1]]], n_bins=n_bins.x, weight=weight, include_outliers=FALSE)
    plot.data.y <- plotting_numerical_buckets(var_to_band=data[[explain_col[2]]], n_bins=n_bins.y, weight=weight, include_outliers=FALSE)

    plot.data <- dplyr::full_join(plot.data.x %>% dplyr::select(lower, upper, center, width) %>% dplyr::mutate(join=1),
                                  plot.data.y %>% dplyr::select(lower, upper, center, width) %>% dplyr::mutate(join=1),
                                  by="join",
                                  suffix=c(".x", ".y")) %>%
      dplyr::select(-join)

    # For each 2D bin
    for (ii in 1:nrow(plot.data)){

      if(ii==1){
        data_ii <- data
      }
      data_ii[[explain_col[[1]]]] <- plot.data[[ii,"center.x"]] # Set the factor to the bucket center
      data_ii[[explain_col[[2]]]] <- plot.data[[ii,"center.y"]] # Set the factor to the bucket center

      #Get predictions
      if(any(class(model)=="xgb.Booster")){
        plot.data[ii,"mean_pred"] <- sum(predict(object=model, newdata=as.matrix(data_ii)) %>% as.vector() * weight) / sum(weight)
      }else{
        plot.data[ii,"mean_pred"] <- sum(predict(object=model, newdata=data_ii) %>% as.vector() * weight) / sum(weight)
      }
    }

    # Plot PDP
    if (use_plotly==TRUE){
      plotly::plot_ly(data=plot.data) %>%
        #add_trace(y=~center.y, x=~center.x, type="contour", z=~mean_pred, contours = list(showlabels = TRUE)) %>%
        plotly::add_trace(x=~center.x, y=~center.y, type="heatmap", z=~mean_pred) %>%
        plotly::layout(
          title = paste0("PDP for factors ", explain_col[1], " & ", explain_col[2]),
          xaxis = list(title=explain_col[1]),
          yaxis = list(title=explain_col[2])
        ) %>%
        plotly::colorbar(title = "Mean prediction") %>%
        return()
    }else{
      out_plot <- ggplot2::ggplot(data=plot.data)+
        ggplot2::geom_tile(ggplot2::aes(x=center.x, y=center.y, fill=mean_pred)) +
        ggplot2::labs(title=paste0("PDP for factors ", explain_col[1], " & ", explain_col[2]),
                      x=explain_col[1],
                      y=explain_col[2],
                      fill="Mean prediction")
      return(out_plot)
    }

  }

}
