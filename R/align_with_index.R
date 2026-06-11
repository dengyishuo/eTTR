#' Align Time Series Data to Target Index
#'
#' Align input time series object to a specified target index sequence.
#' Automatically convert non-xts input to xts, truncate excess rows or pad missing leading observations with fill value.
#'
#' @param x Input time series object, can be numeric vector, matrix or xts/zoo object.
#' @param target_index Vector of date/time index to align \code{x} against; accepts Date, POSIXct, numeric index.
#' @param fill_value Scalar value used to fill newly padded leading rows, default \code{NA}.
#'
#' @details
#' Logic flow:
#' 1. If \code{target_index} is NULL, return original input unchanged.
#' 2. If input is xts with identical index length as target, directly return \code{x}.
#' 3. Non-xts input will be wrapped into an xts object with \code{target_index}, original values placed at the tail, front padded with fill value.
#' 4. For xts input with mismatched index length:
#' \itemize{
#'   \item Shorter input: pad leading rows with fill value, attach target index.
#'   \item Longer input: truncate to match target index length via \code{\link{head}}.
#'   \item Same length different index: overwrite index with target index.
#' }
#'
#' @return An xts object aligned to \code{target_index}.
#'
#' @importFrom xts is.xts xts
#' @importFrom zoo index coredata
#' @export
align_with_index <- function(x, target_index, fill_value = NA) {
  if (is.null(target_index)) {
    return(x)
  }

  # If x is already xts and length matches, return directly
  if (is.xts(x) && length(index(x)) == length(target_index)) {
    return(x)
  }

  # If x is not xts, convert to xts and pad front with fill value
  if (!is.xts(x)) {
    x_xts <- xts(rep(fill_value, length(target_index)), order.by = target_index)
    if (length(x) > 0) {
      x_xts[(length(target_index) - length(x) + 1):length(target_index)] <- x
    }
    return(x_xts)
  }

  # If x is xts but length doesn't match target index length
  current_length <- length(zoo::index(x))
  target_length <- length(target_index)

  if (current_length == target_length) {
    # Same length, replace original index with target index
    zoo::index(x) <- target_index
    return(x)
  } else if (current_length < target_length) {
    # Extend series, fill leading positions with fill_value
    new_x <- xts(rep(fill_value, target_length), order.by = target_index)
    new_x[(target_length - current_length + 1):target_length] <- coredata(x)
    return(new_x)
  } else {
    # Truncate xts to target length
    return(head(x, target_length))
  }
}
