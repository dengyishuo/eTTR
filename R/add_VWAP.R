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
#' @title Calculate Volume - Weighted Average Price (VWAP)
#' @description
#' Calculate the volume - weighted moving average price. The VWAP is a trading benchmark
#' that represents the average price at which a security has traded throughout the day,
#' weighted by the volume traded at each price level. It is commonly used by traders
#' to assess the fair value of a security during a specific period.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' The function will extract the closing price and volume from this object for VWAP calculation.
#' @param n Number of periods to average over. This parameter determines the window size for the VWAP calculation.
#' A larger \code{n} will result in a smoother VWAP, but it may be less responsive to recent price and volume changes.
#' @param append A logical value. If \code{TRUE}, the calculated VWAP values will be appended to the \code{OHLCV} input data,
#' ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
#' VWAP values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
#' containing the VWAP values.
#' If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated VWAP values appended,
#' maintaining the integrity of the time - series alignment.
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' vwap_result1 <- add_VWAP(TSLA)
#'
#' # Using default parameters and appending
#' vwap_result2 <- add_VWAP(TSLA, append = TRUE)
#'
#' # Changing n and without appending
#' vwap_result3 <- add_VWAP(TSLA, n = 15)
#'
#' # Changing n and appending
#' vwap_result4 <- add_VWAP(TSLA, n = 15, append = TRUE)
#' }
add_VWAP <- function(OHLCV, n = 10, append = FALSE) {
  # Check if OHLCV contains 'Close' and 'Volume' columns
  required_cols <- c("Close", "Volume")
  if (!all(required_cols %in% colnames(OHLCV))) {
    stop("OHLCV must contain 'Close' and 'Volume' columns")
  }

  # Extract the closing price and volume
  price <- OHLCV[, "Close"]
  volume <- OHLCV[, "Volume"]
  price <- try.xts(price, error = as.matrix)
  volume <- try.xts(volume, error = as.matrix)

  # Validate the length of volume
  if (length(volume) != length(price)) {
    stop("Length of 'volume' must match the length of 'price'")
  }

  # Calculate the Volume - Weighted Average Price
  res <- WMA(price, n = n, volume)

  if (!is.null(dim(res))) {
    colnames(res) <- "VWAP"
  }

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, res)
    return(combined_result)
  } else {
    return(res)
  }
}
