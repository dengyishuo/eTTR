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

#' @title Moving Window Covariance (runCov)
#' @description
#' Calculate the covariance over a moving window of periods.
#'
#' @param x Object coercible to xts or matrix.
#' @param y Object coercible to xts or matrix (if NULL, use x).
#' @param n Number of periods in the window (1 <= n <= nrow(x)).
#' @param sample Logical; if TRUE, use sample covariance (n-1 denominator).
#' @param cumulative Logical; if TRUE, use from-inception calculation.
#' @return An object of the same class as \code{x} with covariance values.
#' @keywords ts internal
#' @export
#' @examples
#' data(TSLA)
#' cov_20 <- runCov(TSLA[, "Close"], TSLA[, "Volume"], 20)
#' head(cov_20)
runCov <- function(x, y, n = 10, sample = TRUE, cumulative = FALSE) {
  x <- try.xts(x, error = as.matrix)
  y <- try.xts(y, error = as.matrix)
  if (is.xts(x) && is.xts(y)) {
    xy <- cbind(x, y)
  } else {
    xy <- cbind(as.vector(x), as.vector(y))
  }

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1 || NCOL(y) > 1) {
    stop("runCov only supports univariate 'x' and 'y'")
  }

  result <- .Call(runcov, xy[, 1], xy[, 2], n, sample, cumulative)
  reclass(result, x)
}
