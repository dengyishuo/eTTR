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
#' @title Hull Moving Average (HMA)
#' @description
#' Calculate a highly responsive moving average with reduced lag.
#' @param x Price series that is coercible to xts or matrix.
#' @param n Number of periods to average over.
#' @return An object of the same class as \code{x} containing the HMA values.
#' @keywords ts
#' @examples
#' data(TSLA)
#' hma_20 <- HMA(TSLA[, "Close"], 20)
#' head(hma_20)
#' @export
HMA <- function(x, n = 20) {
  # Hull Moving Average
  madiff <- 2 * WMA(x, n = trunc(n / 2)) - WMA(x, n = n)
  hma <- WMA(madiff, n = trunc(sqrt(n)))

  if (!is.null(dim(hma))) {
    colnames(hma) <- "HMA"
  }

  return(hma)
}
