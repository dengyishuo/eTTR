#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2007-2013  Deng Yishuo
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
#' @title Price Oscillator (PO) Calculation
#' @description
#' Calculates the Price Oscillator (PO), a momentum indicator that measures the difference
#' between two moving averages of different periods. It helps identify trends and potential
#' reversal points in financial markets.
#' @param price A price series, which can be a vector, time series, or xts object.
#' @param n_short The period for the short-term moving average, default is 12.
#' @param n_long The period for the long-term moving average, default is 26.
#' @param type The type of return value: "difference" for the absolute difference,
#'             "percent" for the percentage difference. Default is "difference".
#' @param ma_type The type of moving average, supports "SMA" (Simple Moving Average)
#'                and "EMA" (Exponential Moving Average). Default is "SMA".
#' @return A PO indicator series of the same type as the input price series.
#' @details
#' The Price Oscillator (PO) measures momentum by calculating the difference between
#' a short-term and a long-term moving average of prices. A positive PO value indicates
#' upward momentum, while a negative value suggests downward momentum.
#' @references
#' Murphy, J. (1999). Technical Analysis of the Financial Markets. New York Institute of Finance.
#' @seealso
#' \code{\link{SMA}}, \code{\link{EMA}} for moving average calculations.
#' @importFrom utils head
#' @importFrom stats is.ts
#' @importFrom xts xts is.xts
#' @examples
#' # Demonstrate PO calculation with random data
#' set.seed(123)
#' price_data <- xts::xts(rnorm(100, 100, 5), order.by = Sys.Date() - 100:1)
#' # Calculate PO with default parameters (difference form)
#' po_diff <- PO(price_data)
#' # Calculate PO in percentage form
#' po_percent <- PO(price_data, type = "percent")
#' # Calculate PO using EMA
#' po_ema <- PO(price_data, ma_type = "EMA")
#' @export
PO <- function(price, n_short = 12, n_long = 26,
               type = c("difference", "percent"),
               ma_type = c("SMA", "EMA")) {
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
  # Calculate moving averages based on specified type
  if (ma_type == "SMA") {
    # Calculate simple moving averages
    sma_short <- SMA(price, n = n_short)
    sma_long <- SMA(price, n = n_long)
  } else {
    # Calculate exponential moving averages
    sma_short <- EMA(price, n = n_short)
    sma_long <- EMA(price, n = n_long)
  }
  # Calculate PO indicator based on specified type
  if (type == "difference") {
    # Difference form: short MA - long MA
    po <- sma_short - sma_long
  } else {
    # Percentage form: (short MA - long MA) / long MA * 100%
    po <- ((sma_short - sma_long) / sma_long) * 100
  }
  colnames(po) <- "PO"
  # Return the calculated result
  return(po)
}
