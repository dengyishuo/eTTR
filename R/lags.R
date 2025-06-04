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
#
#' @title Lags Calculation
#' @description Computes lagged values of a time series or matrix.
#'
#' @param x Object that is coercible to xts or matrix.
#' @param n Number of periods to lag (default is 1).
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @return A matrix of lagged values with columns labeled by original names and lag periods.
#'
#' @author DengYishuo
#' @keywords ts
#'
#' @examples
#' data <- 1:10
#' lags(data, 2)
#'
#' @export
#'
#'
#' @note This function is deprecated in favor of xts::lag.xts and quantmod::Lag.

lags <- function(x, n = 1) {
  # .Deprecated(c("xts::lag.xts","quantmod::Lag"),"eTTR")

  # Calculate lags of a series
  x <- as.matrix(x)
  if (is.null(colnames(x))) colnames(x) <- paste("V", 1:NCOL(x), sep = "")

  out <- embed(x, n + 1)
  if (n == 1) lag.names <- 1 else if (NCOL(x) == 1) lag.names <- 1:n else lag.names <- rep(1:n, NCOL(x))

  colnames(out) <- c(colnames(x), paste(colnames(x), sort(lag.names), sep = "."))

  return(out)
}
