#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2025 - 2030  DengYishuo
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#' @title Calculate Zero - Lag Exponential Moving Average (ZLEMA)
#' @description
#' Calculate an exponential moving average with reduced lag. The Zero - Lag Exponential Moving
#' Average (ZLEMA) is designed to provide a more timely representation of price trends
#' by reducing the lag typically associated with traditional exponential moving averages.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' The function will extract the closing price from this object for ZLEMA calculation.
#' @param n Number of periods to average over. This parameter affects the smoothness and
#' responsiveness of the ZLEMA. A larger \code{n} will result in a smoother ZLEMA,
#' but it may be less responsive to recent price changes.
#' @param ratio A smoothing/decay ratio. This ratio determines how much weight is given
#' to recent observations in the ZLEMA calculation. If specified, it overrides the
#' default calculation based on \code{n}.
#' @param append A logical value. If \code{TRUE}, the calculated ZLEMA values will be appended to the \code{OHLCV} input data,
#' ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
#' ZLEMA values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
#' containing the ZLEMA values.
#' If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated ZLEMA values appended,
#' maintaining the integrity of the time - series alignment.
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' zlema_result1 <- add_ZLEMA(TSLA)
#'
#' # Using default parameters and appending
#' zlema_result2 <- add_ZLEMA(TSLA, append = TRUE)
#'
#' # Changing n and without appending
#' zlema_result3 <- add_ZLEMA(TSLA, n = 15)
#'
#' # Changing n and appending
#' zlema_result4 <- add_ZLEMA(TSLA, n = 15, append = TRUE)
#' }
add_ZLEMA <- function(OHLCV, n = 10, ratio = NULL, append = FALSE) {
  # Check if OHLCV contains 'Close' column
  if (!"Close" %in% colnames(OHLCV)) {
    stop("OHLCV must contain 'Close' column")
  }

  # Extract the closing price
  x <- OHLCV[, "Close"]
  x <- try.xts(x, error = as.matrix)

  # Validate the number of columns in x
  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. ZLEMA only supports univariate 'x'")
  }

  # If ratio is specified, and n is not, set n to approx 'correct' value
  if (missing(n) && !missing(ratio)) {
    n <- NULL
  }

  # Call C routine (assuming.CALL is defined)
  ma <- .Call(zlema, x, n, ratio)
  ma <- reclass(ma, x)

  if (!is.null(dim(ma))) {
    colnames(ma) <- "ZLEMA"
  }

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, ma)
    return(combined_result)
  } else {
    return(ma)
  }
}
