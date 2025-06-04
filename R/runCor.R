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

#' @title Moving Window Correlation (runCor)
#' @description
#' Calculate the correlation over a moving window of periods.
#'
#' @param x Object coercible to xts or matrix.
#' @param y Object coercible to xts or matrix.
#' @param n Number of periods in the window (1 <= n <= nrow(x)).
#' @param sample Logical; if TRUE, use sample covariance (n-1 denominator).
#' @param cumulative Logical; if TRUE, use from-inception calculation.
#' @return An object of the same class as \code{x} with correlation values.
#' @keywords ts
#' @export
#' @examples
#' data(TSLA)
#' cor_20 <- runCor(TSLA[, "Close"], TSLA[, "Volume"], 20)
#' head(cor_20)
runCor <- function(x, y, n = 10, sample = TRUE, cumulative = FALSE) {
  result <- runCov(x, y, n, sample = sample, cumulative = cumulative) /
    (runSD(x, n, sample = sample, cumulative) *
      runSD(y, n, sample = sample, cumulative))
  return(result)
}
