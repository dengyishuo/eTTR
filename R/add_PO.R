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
#' @title Calculate Price Oscillator (PO)
#' @description
#' Calculates the Price Oscillator (PO), a momentum indicator that measures the difference
#' between two moving averages of different periods. It helps identify trends and potential
#' reversal points in financial markets.
#' @param OHLCV An object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n_short The period for the short - term moving average, default is 12.
#' @param n_long The period for the long - term moving average, default is 26.
#' @param type The type of return value: "difference" for the absolute difference,
#'             "percent" for the percentage difference. Default is "difference".
#' @param ma_type The type of moving average, supports "SMA" (Simple Moving Average)
#'                and "EMA" (Exponential Moving Average). Default is "SMA".
#' @param append A logical value. If \code{TRUE}, the calculated Price Oscillator
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Price Oscillator values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, a PO indicator series of the same type as the input price series (extracted from OHLCV).
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Price Oscillator values appended, maintaining the integrity of the time - series
#' alignment.
#' @details
#' The Price Oscillator (PO) measures momentum by calculating the difference between
#' a short - term and a long - term moving average of prices. A positive PO value indicates
#' upward momentum, while a negative value suggests downward momentum.
#' @references
#' Murphy, J. (1999). Technical Analysis of the Financial Markets. New York Institute of Finance.
#' @seealso
#' \code{\link{SMA}}, \code{\link{EMA}} for moving average calculations.
#' @importFrom utils head
#' @importFrom stats is.ts
#' @importFrom xts xts is.xts
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' po_result1 <- add_PO(TSLA)
#'
#' # Modifying n_short and without appending
#' po_result2 <- add_PO(TSLA, n_short = 14)
#'
#' # Using default parameters and appending
#' po_result3 <- add_PO(TSLA, append = TRUE)
#'
#' # Modifying n_short and appending
#' po_result4 <- add_PO(TSLA, n_short = 14, append = TRUE)
#' }
#' @export
add_PO <- function(OHLCV, n_short = 12, n_long = 26,
                   type = c("difference", "percent"),
                   ma_type = c("SMA", "EMA"), append = FALSE) {
  # Assume we use Close price for calculation, can be adjusted
  price <- OHLCV[, "Close"]

  # Validate input price series
  if (missing(price)) {
    stop("Price series must be provided as input")
  }
  # Check if price series is a vector, time series, or xts object
  if (!is.vector(price) && !is.ts(price) && !inherits(price, "xts")) {
    stop("Price series must be a vector, time series, or xts object")
  }
  # Validate moving average period parameters
  if (!is.numeric(n_short) || !is.numeric(n_long) ||
    n_short <= 0 || n_long <= 0 ||
    n_short >= n_long) {
    stop("Moving average periods must be positive integers, and n_short must be smaller than n_long")
  }
  # Validate return type parameter
  type <- match.arg(type)
  # Validate moving average type parameter
  ma_type <- match.arg(ma_type)

  # Function to calculate moving average
  calculate_ma <- function(p, n, ma_type) {
    if (ma_type == "SMA") {
      return(SMA(p, n = n))
    } else {
      return(EMA(p, n = n))
    }
  }

  # Calculate moving averages
  sma_short <- calculate_ma(price, n_short, ma_type)
  sma_long <- calculate_ma(price, n_long, ma_type)

  # Calculate PO indicator based on specified type
  if (type == "difference") {
    # Difference form: short MA - long MA
    po <- sma_short - sma_long
  } else {
    # Percentage form: (short MA - long MA) / long MA * 100%
    po <- ((sma_short - sma_long) / sma_long) * 100
  }
  colnames(po) <- "PO"

  po <- reclass(po, price)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, po)
    return(combined_result)
  } else {
    return(po)
  }
}
