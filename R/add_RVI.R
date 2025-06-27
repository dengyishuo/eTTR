#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2007 - 2013  Deng Yishuo
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
#' @title Calculate Relative Vigor Index (RVI)
#' @description
#' Computes the Relative Vigor Index (RVI), a momentum oscillator that measures the
#' strength of a trend by comparing closing prices to trading ranges. The RVI is
#' considered bullish when above its signal line and bearish when below.
#' @param OHLCV An object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n The period for calculating standard deviation, default is 14.
#' @param ema.n The period for exponential moving average (EMA) smoothing, default is 3.
#' @param keepNA Logical indicating whether to keep NA values in the result. Default is TRUE.
#' @param append A logical value. If \code{TRUE}, the calculated Relative Vigor Index
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Relative Vigor Index values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same type as the input (vector or xts) containing RVI values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Relative Vigor Index values appended, maintaining the integrity of the time - series
#' alignment.
#' @details
#' The RVI is calculated by:
#' 1. Separating price changes into upward and downward movements
#' 2. Calculating the standard deviation of these movements over a specified period
#' 3. Applying exponential smoothing to the standard deviations
#' 4. Computing the ratio of smoothed upward movements to total movements
#' @references
#' 1. Wilder, J. Welles (1978). New Concepts in Technical Trading Systems.
#' 2. Carver, Constance M. (1992). The New Technical Trader.
#' @seealso
#' \code{\link{EMA}} for exponential moving average calculation.
#' \code{\link{runSD}} for rolling standard deviation calculation.
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' rvi_result1 <- add_RVI(TSLA)
#'
#' # Modifying n and without appending
#' rvi_result2 <- add_RVI(TSLA, n = 20)
#'
#' # Using default parameters and appending
#' rvi_result3 <- add_RVI(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' rvi_result4 <- add_RVI(TSLA, n = 20, append = TRUE)
#' }
#' @export
add_RVI <- function(OHLCV, n = 14, ema.n = 3, keepNA = TRUE, append = FALSE) {
  # Assume we use Close price for calculation, can be adjusted
  price <- OHLCV[, "Close"]
  # Try to convert price to xts, if fails, convert to numeric vector
  price <- try.xts(price, error = function(e) as.numeric(price))

  # Input validation
  if (!is.xts(price) && !is.numeric(price)) {
    stop("price must be an xts object or a numeric vector")
  }

  if (!is.numeric(n) || n <= 0 || !all.equal(n, as.integer(n))) {
    stop("n must be a positive integer")
  }
  n <- as.integer(n)

  if (!is.numeric(ema.n) || ema.n <= 0) {
    stop("ema.n must be a positive numeric value")
  }

  if (!is.logical(keepNA)) {
    stop("keepNA must be a logical value")
  }

  # Function to align data with original index
  align_with_index <- function(data, original_index, fill_value) {
    if (is.xts(data)) {
      data <- merge.xts(data, xts(, original_index), fill = fill_value)
    } else {
      if (length(data) < length(original_index)) {
        data <- c(rep(fill_value, length(original_index) - length(data)), data)
      }
    }
    return(data)
  }

  # Save original timestamp and length
  original_index <- if (is.xts(price)) index(price) else NULL
  original_length <- length(price)

  # Calculate price changes and align timestamps
  price_change <- diff(price)
  price_change <- align_with_index(price_change, original_index, NA)

  # Separate upward and downward changes and align timestamps
  up_change <- ifelse(price_change > 0, price_change, 0)
  down_change <- ifelse(price_change < 0, abs(price_change), 0)

  # Calculate n-period standard deviation and align timestamps
  up_sd <- runSD(up_change, n = n)
  down_sd <- runSD(down_change, n = n)
  up_sd <- align_with_index(up_sd, original_index, NA)
  down_sd <- align_with_index(down_sd, original_index, NA)

  # Apply EMA smoothing to standard deviations and align timestamps
  up_ema <- EMA(up_sd, n = ema.n)
  down_ema <- EMA(down_sd, n = ema.n)
  up_ema <- align_with_index(up_ema, original_index, NA)
  down_ema <- align_with_index(down_ema, original_index, NA)

  # Calculate RVI values, handle division by zero
  rvi <- 100 * (up_ema / (up_ema + down_ema))
  rvi[is.nan(rvi)] <- 0

  # Final alignment
  if (!is.null(original_index)) {
    rvi <- align_with_index(rvi, original_index, NA)
    colnames(rvi) <- "RVI"
  } else {
    if (length(rvi) < original_length) {
      rvi <- c(rep(NA, original_length - length(rvi)), rvi)
    }
    names(rvi) <- "RVI"
  }

  # Decide whether to keep NA values based on keepNA parameter
  if (!keepNA) {
    if (is.xts(rvi)) {
      # NA filtering that preserves index for xts objects
      rvi <- rvi[!is.na(rvi)]
    } else {
      rvi <- stats::na.omit(rvi)
    }
  }

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, rvi)
    return(combined_result)
  } else {
    return(rvi)
  }
}
