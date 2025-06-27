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
#' @title Calculate Exponential Moving Average (EMA)
#' @description
#' Calculate an exponentially - weighted mean, giving more weight to recent observations.
#' The EMA is a type of moving average that responds more quickly to recent price changes
#' compared to a simple moving average.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' The function will extract the closing price from this object for EMA calculation.
#' @param n Number of periods to average over. Must be between 1 and \code{nrow(OHLCV)}.
#' A larger \code{n} will result in a smoother EMA, but it will be less responsive
#' to recent price changes.
#' @param wilder logical; if \code{TRUE}, use Welles Wilder's EMA formula (\code{1/n}).
#' When \code{wilder = TRUE}, the smoothing factor is calculated as \code{1/n},
#' which gives relatively more weight to the most recent data point.
#' @param ratio A smoothing/decay ratio (overrides \code{wilder}). If provided,
#' this ratio is used to calculate the EMA instead of the \code{n}-based approach
#' (when \code{wilder} is \code{FALSE}) or the Welles Wilder's formula (when
#' \code{wilder} is \code{TRUE}).
#' @param append A logical value. If \code{TRUE}, the calculated EMA values will be appended to the \code{OHLCV} input data,
#' ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
#' EMA values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
#' containing the EMA values.
#' If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated EMA values appended,
#' maintaining the integrity of the time - series alignment.
#' @note \code{wilder=FALSE} uses \code{2/(n + 1)}; \code{wilder=TRUE} uses \code{1/n}.
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' ema_result1 <- add_EMA(TSLA)
#'
#' # Using default parameters and appending
#' ema_result2 <- add_EMA(TSLA, append = TRUE)
#'
#' # Changing n and without appending
#' ema_result3 <- add_EMA(TSLA, n = 15)
#'
#' # Changing n and appending
#' ema_result4 <- add_EMA(TSLA, n = 15, append = TRUE)
#' }
add_EMA <- function(OHLCV, n = 10, wilder = FALSE, ratio = NULL, append = FALSE) {
  # Check if OHLCV contains the 'Close' column
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

  # If ratio is specified and n is not, set n to an approximate 'correct' value
  if (missing(n) && !missing(ratio)) {
    n <- NULL
  }

  # Call the C routine to calculate EMA (assuming.CALL(ema) is defined in the package)
  ma <- .Call(ema, close_prices, n, ratio, isTRUE(wilder))

  # Reclassify the result to match the class of OHLCV
  ma <- reclass(ma, close_prices)

  if (!is.null(dim(ma))) {
    colnames(ma) <- "EMA"
  }

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, ma)
    return(combined_result)
  } else {
    return(ma)
  }
}
