#' prep_num_band
#' @description
#' This can band numeric values with a purely numeric vector or a vector of mixed entries.
#' Bands are inclusive in the upper bound.
#' An extra band of 99,999,999 will be added if there are values above the largest band.
#' e.g. If I have a value of 500 and bands of 200, 500 and 800: 500 will be in the 500 band but 500.01 would be in 800.
#'
#' Note: Banding is consistent with how Pricing view bands in rate tables.
#'
#' @section Inputs:
#' @param var_to_band Array[Numeric] - Values that we are trying to apply banding to.
#' @param bands Array[Numeric] - The upper limits of the bands we would like to apply.
#'   Note: this must be sorted in ascending order.
#' @param out_of_bands Numeric (Default = 99,999,999) - This is a value which will get assigned to values above the top band specified.
#'   Note: If you exceed this, it will be replaced by the maximum value of var_to_band or bands (depending on which is larger).
#'   This is intended to be used to have a consistent value across functions for the top band upper limit.
#' @param output Character (Default: "values") - This can accept either "values" or "indices".
#'   Using indices will return the indices of our list of bins to show which value is called.
#'   Note: out_of_bands values will be assigned to the last bin.
#' @return Array[Numeric] - If output == "values": var_to_band but with each value capped into the bands provided.
#'   If output == "indices": Return a list of the indices of the bands which were called.
#' @examples
#' prep_num_band(var_to_band=c(1, 2, 3, 4, 5, 6, 7, 8), bands=c(3, 5, 7))
#' # Output: (3, 3, 3, 5, 5, 7, 7, 99999999)
#'
#' prep_num_band(var_to_band=c(-100, 200, 0, 1.5, 3.14159, 10, 7, -200), bands=c(-150, -10, 4, 5))
#' # Output: (-10, 99999999, 4, 4, 4, 99999999, 99999999, -150)
#'
#' prep_num_band(var_to_band=c(-100, 200, 0, 1.5, 3.14159, 10, 7, -200), bands=c(-150, -10, 4, 5), out_of_bands = 999)
#' # Output: (-10, 999, 4, 4, 4, 999, 999, -150)
#'
#' @export

prep_num_band <- function(var_to_band, bands, out_of_bands=99999999, output="values"){


  # Checking that bands and out_of_bands are numeric. Any.missing checks for NAs.
  checkmate::assert_numeric(bands, any.missing = FALSE)
  checkmate::assert_numeric(out_of_bands,any.missing = FALSE, len = 1)

  # Checking that banding makes sense and is sorted.
  checkmate::assert_set_equal(bands,sort(bands),ordered = TRUE)

  # Checking that the amount of values to be banded is more than 0.
  checkmate::assert_true(!is.null(var_to_band))

  if (!(output %in% c("values","indices"))) {
    warning(paste0("WARNING: Output is not equal to values or indices. Output value: ",output))
  }
  checkmate::assert(output %in% c("values","indices"))

  # Warning for if only 1 unique value.
  if (length(unique(var_to_band)) == 1) {
    warning("WARNING: Only 1 unique value")
  }

  # This is getting the numeric parts of the array.
  var_to_band_num <- var_to_band[!is.na(suppressWarnings(as.numeric(var_to_band)))]
  var_to_band_num <- as.numeric(var_to_band_num)

  # This will enforce that if largest value of out_of_bands, bands, var_to_band_num will become out_of_bands if the others exceed it.
  if (max(var_to_band_num,bands) > out_of_bands) {
    warning(paste0("WARNING: New out_of_bands assigned = ",max(var_to_band_num, bands)))
  }
  out_of_bands <- max(out_of_bands, bands, var_to_band_num)

  # Adding a max value. If one already exists, nothing from this band will get returned (assuming it is higher).
  bands[length(bands) + 1] <- out_of_bands
  if (is.array(var_to_band)) {
    # Arrays by default do not have dimension. This assigns it as needed for apply.
    dim(var_to_band_num) <- length(var_to_band_num)
  }

  # Finds the index of the first band which is larger than the element in consideration but we retrieve this for the entire array.
  indices <- vapply(var_to_band_num,function(x) which(x <= bands)[1], FUN.VALUE = numeric(1))

  if (output == "values") {
    # Replacing the numeric parts with the banded values.
    var_to_band[!is.na(suppressWarnings(as.numeric(var_to_band)))] <- bands[indices]
    return(var_to_band) # This returns the value of bands for each of the positions.
  } else
  if (output == "indices") {
    # Convert all of those which are non-numeric to NA and those which are to the indices
    var_to_band[is.na(suppressWarnings(as.numeric(var_to_band)))] <- NA
    var_to_band[!is.na(suppressWarnings(as.numeric(var_to_band)))] <- indices
    var_to_band <- as.numeric(var_to_band)
    return(var_to_band)
  }
  # TODO: Add decreasing or unsorted bandings.
}

var_to_band <- c("A","B",1,4,6,7,23,3,7,12,12,4,123)
bands <- c(2,4,8,11)
out_of_bands <- 99999999
output <- "values"
