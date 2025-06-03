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

#' Moving Window Minimum (runMin)
#'
#' Calculate the minimum value over a moving window of periods.
#'
#' @param x Object coercible to xts or matrix.
#' @param n Number of periods in the window (1 <= n <= nrow(x)).
#' @param cumulative Logical; if TRUE, use from-inception cumulative min.
#' @return An object of the same class as \code{x} with minimum values.
#' @useDynLib eTTR, .registration = TRUE
#' @keywords ts internal
#' @export
#' @examples
#' data(ttrc)
#' min_10 <- runMin(ttrc[, "Close"], 10)
#' head(min_10)
runMin <- function(x, n = 10, cumulative = FALSE) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. runMin only supports univariate 'x'")
  }

  if (cumulative) {
    NAs <- sum(is.na(x))
    if (NAs > 0) {
      if (any(is.na(x[-(1:NAs)]))) stop("Series contains non-leading NAs")
      if (NAs + n > NROW(x)) stop("not enough non-NA values")
    }
    beg <- 1 + NAs
    result <- double(NROW(x))
    result[beg:NROW(x)] <- cummin(x[beg:NROW(x)])
    is.na(result) <- seq_len(n - 1 + NAs)
  } else {
    result <- .Call(C_runmin, x, n)
  }

  reclass(result, x)
}
