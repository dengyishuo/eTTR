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

#' @title Welles Wilder Style Sum (wilderSum)
#' @description
#' Calculate a weighted sum using Welles Wilder's method.
#'
#' @param x Object coercible to xts or matrix.
#' @param n Number of periods in the window (1 <= n <= nrow(x)).
#' @return An object of the same class as \code{x} with Wilder-style sums.
#' @useDynLib eTTR, .registration = TRUE
#' @keywords ts internal
#' @export
#' @examples
#'
#' data(TSLA)
#' wilder_14 <- wilderSum(TSLA[, "High"] - TSLA[, "Low"], 14)
#' head(wilder_14)
wilderSum <- function(x, n = 10) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. wilderSum only supports univariate 'x'")
  }

  naCheck(x, n) # Assume naCheck is a helper function
  result <- .Call(C_wilderSum, x, n)
  reclass(result, x)
}
