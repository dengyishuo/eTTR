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

#' Moving Window Mean (runMean)
#'
#' Calculate the mean value over a moving window of periods.
#'
#' @param x Object coercible to xts or matrix.
#' @param n Number of periods in the window (1 <= n <= nrow(x)).
#' @param cumulative Logical; if TRUE, use from-inception cumulative mean.
#' @return An object of the same class as \code{x} with mean values.
#' @keywords ts
#' @export
#' @examples
#' data(ttrc)
#' mean_10 <- runMean(ttrc[, "Close"], 10)
#' head(mean_10)
runMean <- function(x, n = 10, cumulative = FALSE) {
  if (cumulative) {
    x.na <- sum(is.na(x))
    denom <- c(rep(NA_real_, x.na), seq_len(NROW(x) - x.na))
    result <- runSum(x, n, cumulative) / denom
  } else {
    result <- runSum(x, n) / n
  }
  return(result)
}
