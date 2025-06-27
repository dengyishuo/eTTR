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
#' @title Calculate Simple Moving Average (SMA)
#' @description
#' Calculate the arithmetic mean of the series over the past \code{n} observations.
#' The Simple Moving Average (SMA) is a basic technical analysis tool that provides
#' a smoothed view of the price trend by averaging the closing prices over a
#' specified number of periods.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' The function will extract the closing price from this object for SMA calculation.
#' @param n Number of periods to average over. Must be between 1 and \code{nrow(OHLCV)}.
#' A larger \code{n} results in a smoother SMA, but it may be less responsive to recent price changes.
#' @param append A logical value. If \code{TRUE}, the calculated SMA values will be appended to the \code{OHLCV} input data,
#' ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
#' SMA values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
#' containing the SMA values.
#' If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated SMA values appended,
#' maintaining the integrity of the time - series alignment.
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' sma_result1 <- add_SMA(TSLA)
#'
#' # Using default parameters and appending
#' sma_result2 <- add_SMA(TSLA, append = TRUE)
#'
#' # Changing n and without appending
#' sma_result3 <- add_SMA(TSLA, n = 15)
#'
#' # Changing n and appending
#' sma_result4 <- add_SMA(TSLA, n = 15, append = TRUE)
#' }
add_SMA <- function(OHLCV, n = 10, append = FALSE) {
  # Check if OHLCV contains 'Close' column
  if (!"Close" %in% colnames(OHLCV)) {
    stop("OHLCV must contain 'Close' column")
  }

  # Extract the closing price
  close_prices <- OHLCV[, "Close"]
  close_prices <- try.xts(close_prices, error = as.matrix)

  # Validate the parameter n
  if (n < 1 || n > NROW(close_prices)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(close_prices)))
  }

  # Calculate the Simple Moving Average
  ma <- runMean(close_prices, n)

  if (!is.null(dim(ma))) {
    colnames(ma) <- "SMA"
  }

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, ma)
    return(combined_result)
  } else {
    return(ma)
  }
}
