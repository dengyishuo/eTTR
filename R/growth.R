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
#' @title Calculate Investment Growth
#' @description
#' This function computes the growth of an investment based on given prices
#' and trading signals. It assumes an initial investment of $1 and applies
#' the provided signals to determine positions (long, short, or none).
#'
#' @param price Price series that is coercible to xts or matrix.
#' @param signals Signals to use (defaults to vector of ones). Use '0' for no position,
#'                '1' for long position, and '-1' for short position.
#' @param ... Further arguments to be passed to ROC for return calculation.
#'
#' @return A vector of the cumulative growth of the investment over time.
#' @note You can specify the number of periods and type of compounding
#'       via the '\code{...}' argument when calculating returns.
#' @author DengYishuo
#' @keywords ts
#' @examples
#' prices <- c(100, 105, 103, 107, 110)
#' signals <- c(1, 1, -1, 1, 1)
#' growth(prices, signals)
#' @export
growth <- function(price, signals, ...) {
  # Calculate growth of $1 for a series of returns (and signals).

  if (missing(signals)) {
    signals <- rep(1, NROW(price))
  } else {
    signals <- as.vector(signals)
  }
  price <- as.vector(price)
  growth <- cumprod(1 + ROC(price, ...) * signals)

  return(growth)
}
