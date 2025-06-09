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
#' @title Volume-Weighted Average Price (VWAP)
#' @description
#' Calculate the volume-weighted moving average price.
#'
#' @param price Price series that is coercible to xts or matrix.
#' @param volume Volume series that matches the length of \code{price}.
#' @param n Number of periods to average over.
#' @return An object of the same class as \code{price} containing the VWAP values.
#' @keywords ts
#' @export
#' @examples
#' data(TSLA)
#' vwap_20 <- VWAP(TSLA[, "Close"], TSLA[, "Volume"], 20)
#' head(vwap_20)
VWAP <- VWMA <- function(price, volume, n = 10) {
  # Volume-weighted average price / moving average
  res <- WMA(price, n = n, volume)
  if (!is.null(dim(res))) {
    colnames(res) <- "VWAP"
  }
  return(res)
}
