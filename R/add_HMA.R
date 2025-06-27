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
#' @title Calculate Hull Moving Average (HMA)
#' @description
#' Calculate a highly responsive moving average with reduced lag. The Hull Moving
#' Average (HMA) is designed to provide a more accurate representation of price
#' trends by reducing the lag typically associated with traditional moving averages.
#' It does this by using a weighted combination of different - period moving averages.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' The function will extract the closing price from this object for HMA calculation.
#' @param n Number of periods to average over. This parameter determines the overall
#' window size for the HMA calculation. A larger \code{n} will result in a smoother
#' HMA, but it may be less responsive to short - term price changes.
#' @param append A logical value. If \code{TRUE}, the calculated HMA values will be appended to the \code{OHLCV} input data,
#' ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
#' HMA values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
#' containing the HMA values.
#' If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated HMA values appended,
#' maintaining the integrity of the time - series alignment.
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' hma_result1 <- add_HMA(TSLA)
#'
#' # Using default parameters and appending
#' hma_result2 <- add_HMA(TSLA, append = TRUE)
#'
#' # Changing n and without appending
#' hma_result3 <- add_HMA(TSLA, n = 15)
#'
#' # Changing n and appending
#' hma_result4 <- add_HMA(TSLA, n = 15, append = TRUE)
#' }
add_HMA <- function(OHLCV, n = 20, append = FALSE) {
  # Check if OHLCV contains 'Close' column
  if (!"Close" %in% colnames(OHLCV)) {
    stop("OHLCV must contain 'Close' column")
  }

  # Extract the closing price
  x <- OHLCV[, "Close"]
  x <- try.xts(x, error = as.matrix)

  # Validate the parameter n
  if (n < 1) {
    stop("n must be a positive integer.")
  }

  # Calculate intermediate moving averages
  madiff <- 2 * WMA(x, n = trunc(n / 2)) - WMA(x, n = n)
  hma <- WMA(madiff, n = trunc(sqrt(n)))

  if (!is.null(dim(hma))) {
    colnames(hma) <- "HMA"
  }

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, hma)
    return(combined_result)
  } else {
    return(hma)
  }
}
