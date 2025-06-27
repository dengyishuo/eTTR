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
#' @title Calculate True Strength Index (TSI)
#' @description
#' The True Strength Index (TSI) is a momentum - based oscillator that measures the strength
#' of price trends. It uses double exponential moving averages (EMAs) of price changes and
#' their absolute values to calculate an oscillator value. This function computes the TSI
#' and an optional signal line (an EMA of the TSI) from the closing prices within the OHLCV data.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#'              The function will extract the closing price from this object for TSI calculation.
#' @param r The period for the first exponential moving average (fast EMA), default is 13.
#'          A shorter period makes the fast EMA more responsive to recent price changes,
#'          while a longer period smooths out the data more.
#' @param s The period for the second exponential moving average (slow EMA), default is 25.
#'          Similar to \code{r}, it affects the responsiveness and smoothness of the slow EMA.
#' @param signal_period The period for the signal line (EMA of TSI), default is 9.
#'                      This determines how smooth the signal line is, with a shorter period
#'                      making it more responsive to changes in the TSI.
#' @param append A logical value. If \code{TRUE}, the calculated TSI and signal line
#'               values will be appended to the input \code{OHLCV} data (if it's an xts object), ensuring
#'               proper alignment of time - series data. If \code{FALSE}, only the calculated
#'               TSI and signal line values will be returned. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to the EMA function (e.g., wilder).
#'           These can be used to customize the EMA calculation, such as using Welles Wilder's
#'           EMA formula if \code{wilder = TRUE}.
#' @return
#' If \code{append = FALSE}, an object of the same type as the input (vector or xts) containing two columns:
#' "tsi" (True Strength Index values) and "signal" (signal line values).
#' If \code{append = TRUE} and the input \code{OHLCV} is an xts object, an xts object with the calculated
#' TSI and signal line values appended, maintaining the integrity of the time - series alignment.
#' If \code{append = TRUE} and the input \code{OHLCV} is not an xts object, an error is thrown.
#' @export
add_TSI <- function(OHLCV, r = 13, s = 25, signal_period = 9, append = FALSE, ...) {
  # Check if OHLCV contains 'Close' column
  if (!"Close" %in% colnames(OHLCV)) {
    stop("OHLCV must contain 'Close' column")
  }

  # Extract the closing price
  price_data <- OHLCV[, "Close"]
  price_data <- as.numeric(price_data)
  original_index <- if (inherits(OHLCV, "xts")) index(OHLCV) else NULL

  # Calculate price changes
  price_change <- diff(price_data)

  # Calculate EMAs
  ema1 <- EMA(price_change, n = r, ...)
  ema2 <- EMA(ema1, n = s, ...)
  abs_ema1 <- EMA(abs(price_change), n = r, ...)
  abs_ema2 <- EMA(abs_ema1, n = s, ...)

  # Calculate TSI
  tsi <- (ema2 / abs_ema2) * 100
  signal <- EMA(tsi, n = signal_period, ...)

  # Combine TSI and signal into a single object
  result <- cbind(tsi, signal)
  colnames(result) <- c("tsi", "signal")

  # Handle result alignment for xts objects
  if (inherits(OHLCV, "xts")) {
    # Determine the valid indices for the result based on EMA calculations
    valid_start <- max(r, s) + 1
    valid_indices <- seq(valid_start, length(price_data), by = 1)
    valid_indices <- valid_indices[seq_len(nrow(result))]
    result <- xts(result, order.by = original_index[valid_indices])
  }

  if (append) {
    if (!inherits(OHLCV, "xts")) {
      stop("Append option is only available for xts input")
    }
    # Avoid column name conflicts
    new_colnames <- paste0(c("tsi", "signal"), "_tsi")
    colnames(result) <- new_colnames
    combined_result <- cbind(OHLCV, result)
    return(combined_result)
  } else {
    return(result)
  }
}
