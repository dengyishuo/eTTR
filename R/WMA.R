#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2025-2030  DengYishuo
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
#' @title Weighted Moving Average (WMA)
#' @description
#' Calculate a weighted moving average with custom weights.
#' @param x Price series that is coercible to xts or matrix.
#' @param n Number of periods to average over. Must be between 1 and \code{nrow(x)}.
#' @param wts Vector of weights (length must equal \code{n} or length of \code{x}).
#' @return An object of the same class as \code{x} containing the WMA values.
#' @keywords ts
#' @export
#' @examples
#' data(TSLA)
#' wma_10 <- WMA(TSLA[, "Close"], 10)
#' head(wma_10)
WMA <- function(x, n = 10, wts = 1:n) {
  # Weighted Moving Average
  x <- try.xts(x, error = as.matrix)
  wts <- try.xts(wts, error = as.matrix)
  if (!any(NROW(wts) == c(NROW(x), n))) {
    stop("Length of 'wts' must equal the length of 'x' or 'n'")
  }
  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }
  if (NCOL(x) > 1 || NCOL(wts) > 1) {
    stop("WMA only supports univariate 'x' and 'wts'")
  }
  # Calculate leading NAs using cumulative product method
  leading_na_x <- sum(cumprod(is.na(x)))
  leading_na_wts <- if (NROW(wts) == NROW(x)) sum(cumprod(is.na(wts))) else 0

  # Check for non-leading NAs in x
  if (leading_na_x < length(x) && anyNA(x[(leading_na_x + 1):length(x)])) {
    stop("'x' contains non-leading NAs")
  }

  # Check for non-leading NAs in wts when length matches x
  if (NROW(wts) == NROW(x) &&
    leading_na_wts < length(wts) &&
    anyNA(wts[(leading_na_wts + 1):length(wts)])) {
    stop("'wts' contains non-leading NAs")
  }

  # Handle weight vector case
  if (NROW(wts) == n) {
    if (anyNA(wts)) {
      stop("'wts' vector of length 'n' cannot have NA values")
    }
    # Call C routine (assuming .Call is defined)
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

  return(ma)
}
