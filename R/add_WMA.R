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
#' @title Calculate Weighted Moving Average (WMA)
#' @description
#' Calculate a weighted moving average with custom weights. The WMA assigns different
#' weights to each data point in the series, allowing for more flexibility in
#' emphasizing recent or past data.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' The function will extract the closing price from this object for WMA calculation.
#' @param n Number of periods to average over. Must be between 1 and \code{nrow(OHLCV)}.
#' A larger \code{n} will result in a smoother WMA, but it may be less responsive to recent price changes.
#' @param wts Vector of weights (length must equal \code{n} or length of \code{OHLCV}).
#' These weights determine the importance of each data point in the moving average calculation.
#' @param append A logical value. If \code{TRUE}, the calculated WMA values will be appended to the \code{OHLCV} input data,
#' ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
#' WMA values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
#' containing the WMA values.
#' If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated WMA values appended,
#' maintaining the integrity of the time - series alignment.
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' wma_result1 <- add_WMA(TSLA)
#'
#' # Using default parameters and appending
#' wma_result2 <- add_WMA(TSLA, append = TRUE)
#'
#' # Changing n and without appending
#' wma_result3 <- add_WMA(TSLA, n = 15)
#'
#' # Changing n and appending
#' wma_result4 <- add_WMA(TSLA, n = 15, append = TRUE)
#' }
add_WMA <- function(OHLCV, n = 10, wts = 1:n, append = FALSE) {
  # Check if OHLCV contains 'Close' column
  if (!"Close" %in% colnames(OHLCV)) {
    stop("OHLCV must contain 'Close' column")
  }

  # Extract the closing price
  x <- OHLCV[, "Close"]
  x <- try.xts(x, error = as.matrix)
  wts <- try.xts(wts, error = as.matrix)

  # Validate the length of wts
  if (!any(NROW(wts) == c(NROW(x), n))) {
    stop("Length of 'wts' must equal the length of 'x' or 'n'")
  }

  # Validate the parameter n
  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  # Validate the number of columns in x and wts
  if (NCOL(x) > 1 || NCOL(wts) > 1) {
    stop("WMA only supports univariate 'x' and 'wts'")
  }

  # Calculate leading NAs using cumulative product method
  leading_na_x <- sum(cumprod(is.na(x)))
  leading_na_wts <- if (NROW(wts) == NROW(x)) sum(cumprod(is.na(wts))) else 0

  # Check for non - leading NAs in x
  if (leading_na_x < length(x) && anyNA(x[(leading_na_x + 1):length(x)])) {
    stop("'x' contains non - leading NAs")
  }

  # Check for non - leading NAs in wts when length matches x
  if (NROW(wts) == NROW(x) &&
    leading_na_wts < length(wts) &&
    anyNA(wts[(leading_na_wts + 1):length(wts)])) {
    stop("'wts' contains non - leading NAs")
  }

  # Handle weight vector case
  if (NROW(wts) == n) {
    if (anyNA(wts)) {
      stop("'wts' vector of length 'n' cannot have NA values")
    }
    # Call C routine (assuming.CALL is defined)
    ma <- .Call(wma, x, wts, n)
  } else {
    xw <- cbind(x, wts)
    ma <- runSum(xw[, 1] * xw[, 2], n) / runSum(xw[, 2], n)
  }

  ma[1:(n - 1)] <- NA
  ma <- reclass(ma, x)

  if (!is.null(dim(ma))) {
    colnames(ma) <- "WMA"
  }

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, ma)
    return(combined_result)
  } else {
    return(ma)
  }
}
