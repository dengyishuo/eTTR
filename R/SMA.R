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

#' @title Simple Moving Average (SMA)
#' @description
#' Calculate the arithmetic mean of the series over the past \code{n} observations.
#'
#' @param x Price series that is coercible to xts or matrix.
#' @param n Number of periods to average over. Must be between 1 and \code{nrow(x)}.
#' @return An object of the same class as \code{x} containing the SMA values.
#' @keywords ts
#' @export
#' @examples
#' data(TSLA)
#' sma_20 <- SMA(TSLA[, "Close"], 20)
#' head(sma_20)
SMA <- function(x, n = 10) {
  # Simple Moving Average
  ma <- runMean(x, n)

  if (!is.null(dim(ma))) {
    colnames(ma) <- "SMA"
  }

  return(ma)
}
