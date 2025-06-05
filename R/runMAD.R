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

#' @title Moving Window Median Absolute Deviation (runMAD)
#' @description
#' Calculate the median absolute deviation over a moving window of periods.
#'
#' @param x Object coercible to xts or matrix.
#' @param n Number of periods in the window (1 <= n <= nrow(x)).
#' @param center Central tendency to use (NULL for median).
#' @param stat Type of MAD: 'median' (default) or 'mean'.
#' @param constant Scale factor for median MAD (default 1.4826).
#' @param non.unique Handling of even-sized windows: 'mean', 'max', or 'min'.
#' @param cumulative Logical; if TRUE, use from-inception calculation.
#' @return An object of the same class as \code{x} with MAD values.
#' @keywords ts internal
#' @export
#' @examples
#' data(TSLA)
#' mad_20 <- runMAD(TSLA[, "Close"], 20)
#' head(mad_20)
runMAD <- function(x, n = 10, center = NULL, stat = "median",
                   constant = 1.4826, non.unique = "mean", cumulative = FALSE) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. runMAD only supports univariate 'x'")
  }

  if (is.null(center)) {
    center <- runMedian(x, n, cumulative = cumulative)
  }

  median <- match.arg(stat, c("mean", "median"))
  median <- switch(stat,
    median = TRUE,
    mean = FALSE
  )

  non.unique <- match.arg(non.unique, c("mean", "max", "min"))
  non.unique <- switch(non.unique,
    mean = 0,
    max = 1,
    min = -1
  )

  result <- .Call(C_runmad, x, center, n, median, non.unique, cumulative)
  if (median) result <- result * constant
  reclass(result, x)
}
