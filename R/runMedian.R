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

#' Moving Window Median (runMedian)
#'
#' Calculate the median value over a moving window of periods.
#'
#' @param x Object coercible to xts or matrix.
#' @param n Number of periods in the window (1 <= n <= nrow(x)).
#' @param non.unique Handling of even-sized windows: 'mean', 'max', or 'min'.
#' @param cumulative Logical; if TRUE, use from-inception calculation.
#' @return An object of the same class as \code{x} with median values.
#' @useDynLib eTTR, .registration = TRUE
#' @keywords ts internal
#' @export
#' @examples
#' data(ttrc)
#' median_10 <- runMedian(ttrc[, "Close"], 10)
#' head(median_10)
runMedian <- function(x, n = 10, non.unique = "mean", cumulative = FALSE) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. runMedian only supports univariate 'x'")
  }

  non.unique <- match.arg(non.unique, c("mean", "max", "min"))
  non.unique <- switch(non.unique,
    mean = 0L,
    max = 1L,
    min = -1L
  )

  result <- .Call(C_runmedian, x, n, non.unique, cumulative)
  reclass(result, x)
}
