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
#' @title Calculate Elastic Volume - Weighted Moving Average (EVWMA)
#' @description
#' Calculate a volume - weighted moving average that adjusts to trading volume.
#' The EVWMA takes into account the trading volume when calculating the moving average,
#' giving more importance to periods with higher volume.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' The function will extract the closing price and volume from this object for EVWMA calculation.
#' @param n Number of periods to average over. This determines the window size for the moving average calculation.
#' A larger \code{n} will result in a smoother EVWMA, but it may be less responsive to recent price changes.
#' @param append A logical value. If \code{TRUE}, the calculated EVWMA values will be appended to the \code{OHLCV} input data,
#' ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
#' EVWMA values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
#' containing the EVWMA values.
#' If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated EVWMA values appended,
#' maintaining the integrity of the time - series alignment.
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' evwma_result1 <- add_EVWMA(TSLA)
#'
#' # Using default parameters and appending
#' evwma_result2 <- add_EVWMA(TSLA, append = TRUE)
#'
#' # Changing n and without appending
#' evwma_result3 <- add_EVWMA(TSLA, n = 15)
#'
#' # Changing n and appending
#' evwma_result4 <- add_EVWMA(TSLA, n = 15, append = TRUE)
#' }
add_EVWMA <- function(OHLCV, n = 10, append = FALSE) {
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
  if (!any(NROW(volume) == c(NROW(price), 1))) {
    stop("Length of 'volume' must equal 1 or the length of 'price'")
  }

  # Validate the parameter n
  if (n < 1 || n > NROW(price)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(price)))
  }

  # Validate the number of columns in price and volume
  if (NCOL(price) > 1 || NCOL(volume) > 1) {
    stop("EVWMA only supports univariate 'price' and 'volume'")
  }

  pv <- cbind(price, volume)
  if (any(n > colSums(!is.na(pv)))) {
    stop("n > number of non - NA values in price/volume")
  }

  # Call the C routine to calculate EVWMA (assuming.CALL(evwma) is defined)
  ma <- .Call(evwma, pv[, 1], pv[, 2], n)
  ma <- reclass(ma, price)

  if (!is.null(dim(ma))) {
    colnames(ma) <- "EVWMA"
  }

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, ma)
    return(combined_result)
  } else {
    return(ma)
  }
}
