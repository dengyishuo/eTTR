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

#' @title Exponential Moving Average (EMA)
#' @description
#' Calculate an exponentially-weighted mean, giving more weight to recent observations.
#'
#' @param x Price series that is coercible to xts or matrix.
#' @param n Number of periods to average over. Must be between 1 and \code{nrow(x)}.
#' @param wilder logical; if \code{TRUE}, use Welles Wilder's EMA formula (\code{1/n}).
#' @param ratio A smoothing/decay ratio (overrides \code{wilder}).
#' @return An object of the same class as \code{x} containing the EMA values.
#' @note \code{wilder=FALSE} uses \code{2/(n+1)}; \code{wilder=TRUE} uses \code{1/n}.
#' @keywords ts
#' @useDynLib eTTR, .registration = TRUE
#' @export
#' @examples
#'
#' data(TSLA)
#' ema_20 <- EMA(TSLA[, "Close"], 20)
#' head(ema_20)
EMA <- function(x, n = 10, wilder = FALSE, ratio = NULL) {
  # Exponential Moving Average
  x <- try.xts(x, error = as.matrix)
  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  # If ratio is specified, and n is not, set n to approx 'correct' value
  if (missing(n) && !missing(ratio)) {
    n <- NULL
  }

  # Call C routine (assuming .Call is defined in package)
  ma <- .Call(C_ema, x, n, ratio, isTRUE(wilder))
  ma <- reclass(ma, x)

  if (!is.null(dim(ma))) {
    colnames(ma) <- "EMA"
  }

  return(ma)
}
