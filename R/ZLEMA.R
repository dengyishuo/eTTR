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

#' @title Zero-Lag Exponential Moving Average (ZLEMA)
#' @description
#' Calculate an exponential moving average with reduced lag.
#'
#' @param x Price series that is coercible to xts or matrix.
#' @param n Number of periods to average over.
#' @param ratio A smoothing/decay ratio.
#' @return An object of the same class as \code{x} containing the ZLEMA values.
#' @keywords ts
#' @export
#' @examples
#'
#' data(TSLA)
#' zlema_20 <- ZLEMA(TSLA[, "Close"], 20)
#' head(zlema_20)
ZLEMA <- function(x, n = 10, ratio = NULL) {
  # Zero-Lag Exponential Moving Average
  x <- try.xts(x, error = as.matrix)
  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. ZLEMA only supports univariate 'x'")
  }

  # If ratio is specified, and n is not, set n to approx 'correct' value
  if (missing(n) && !missing(ratio)) {
    n <- NULL
  }

  # Call C routine (assuming .Call is defined)
  ma <- .Call(C_zlema, x, n, ratio)
  ma <- reclass(ma, x)

  if (!is.null(dim(ma))) {
    colnames(ma) <- "ZLEMA"
  }

  return(ma)
}
