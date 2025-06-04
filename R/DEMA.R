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

#' @title Double Exponential Moving Average (DEMA)
#' @description
#' Calculate a double exponential moving average to reduce lag.
#'
#' @param x Price series that is coercible to xts or matrix.
#' @param n Number of periods to average over. Must be between 1 and \code{nrow(x)}.
#' @param v The 'volume factor' (a number in [0,1]); default is 1 (standard DEMA).
#' @param wilder logical; if \code{TRUE}, use Welles Wilder's EMA formula.
#' @param ratio A smoothing/decay ratio (overrides \code{wilder}).
#' @return An object of the same class as \code{x} containing the DEMA values.
#' @note When \code{v=1}, returns standard DEMA; \code{v=0} returns regular EMA.
#' @keywords ts
#' @export
#' @examples
#' data(TSLA)
#' dema_20 <- DEMA(TSLA[, "Close"], 20)
#' head(dema_20)
DEMA <- function(x, n = 10, v = 1, wilder = FALSE, ratio = NULL) {
  # Double Exponential Moving Average
  x <- try.xts(x, error = as.matrix)
  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }
  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. DEMA only supports univariate 'x'")
  }
  if (v < 0 || v > 1) {
    stop("Please ensure 0 <= v <= 1")
  }

  if (missing(n) && !missing(ratio)) {
    n <- NULL
  }

  # Call C routine for EMA (assuming .Call is defined)
  ma1 <- .Call(C_ema, x, n, ratio, isTRUE(wilder))
  d <- .Call(C_ema, ma1, n, ratio, isTRUE(wilder))

  dema <- (1 + v) * ma1 - d * v
  dema <- reclass(dema, x)

  if (!is.null(dim(dema))) {
    colnames(dema) <- "DEMA"
  }

  return(dema)
}
