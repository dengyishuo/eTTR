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
#' @title Calculate Double Exponential Moving Average (DEMA)
#' @description
#' Calculate a double exponential moving average to reduce lag. The DEMA is designed
#' to provide a more responsive moving average compared to the simple exponential
#' moving average (EMA) by applying the EMA calculation twice.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' The function will extract the closing price from this object for DEMA calculation.
#' @param n Number of periods to average over. Must be between 1 and \code{nrow(OHLCV)}.
#' A larger \code{n} will result in a smoother DEMA, but it may be less responsive
#' to recent price changes.
#' @param v The 'volume factor' (a number in \code{[0,1]}; default is 1 (standard DEMA).
#' When \code{v = 1}, the function returns the standard DEMA. When \code{v = 0},
#' it returns the regular EMA. This parameter allows for adjusting the influence
#' of the second EMA calculation on the final DEMA value.
#' @param wilder logical; if \code{TRUE}, use Welles Wilder's EMA formula. Welles
#' Wilder's EMA formula gives more weight to recent data points in a specific way.
#' @param ratio A smoothing/decay ratio (overrides \code{wilder}). If provided,
#' this ratio is used to calculate the EMA instead of the \code{n}-based approach
#' (when \code{wilder} is \code{FALSE}) or the Welles Wilder's formula (when
#' \code{wilder} is \code{TRUE}).
#' @param append A logical value. If \code{TRUE}, the calculated DEMA values will be appended to the \code{OHLCV} input data,
#' ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
#' DEMA values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
#' containing the DEMA values.
#' If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated DEMA values appended,
#' maintaining the integrity of the time - series alignment.
#' @note When \code{v = 1}, returns standard DEMA; \code{v = 0} returns regular EMA.
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' dema_result1 <- add_DEMA(TSLA)
#'
#' # Using default parameters and appending
#' dema_result2 <- add_DEMA(TSLA, append = TRUE)
#'
#' # Changing n and without appending
#' dema_result3 <- add_DEMA(TSLA, n = 15)
#'
#' # Changing n and appending
#' dema_result4 <- add_DEMA(TSLA, n = 15, append = TRUE)
#' }
add_DEMA <- function(OHLCV, n = 10, v = 1, wilder = FALSE, ratio = NULL, append = FALSE) {
  # Check if OHLCV contains the 'Close' column
  if (!"Close" %in% colnames(OHLCV)) {
    stop("OHLCV must contain 'Close' column")
  }
  # Extract the closing price
  close_prices <- OHLCV[, "Close"]
  close_prices <- try.xts(close_prices, error = as.matrix)

  # Parameter validation
  if (n < 1 || n > NROW(close_prices)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(close_prices)))
  }
  if (v < 0 || v > 1) {
    stop("Please ensure 0 <= v <= 1")
  }
  if (missing(n) && !missing(ratio)) {
    n <- NULL
  }

  # Call the C routine to calculate the first EMA (assuming.CALL(ema) is defined)
  ma1 <- .Call(ema, close_prices, n, ratio, isTRUE(wilder))
  # Call the C routine again to calculate the second EMA
  d <- .Call(ema, ma1, n, ratio, isTRUE(wilder))

  # Calculate DEMA according to the formula
  dema <- (1 + v) * ma1 - d * v
  dema <- reclass(dema, close_prices)

  if (!is.null(dim(dema))) {
    colnames(dema) <- "DEMA"
  }

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, dema)
    return(combined_result)
  } else {
    return(dema)
  }
}
