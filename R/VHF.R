#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2007-2013  Deng Yishuo
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

#' @title Vertical Horizontal Filter (Optimized)
#' @description
#' The Vertical Horizontal Filter (VHF) attempts to identify starting and ending
#' trends. This optimized version includes safeguards against NA and INF values.
#'
#' @param price Object that is coercible to xts or matrix and contains a Close
#' price series, or a High-Low-Close price series.
#' @param n Number of periods to use. Default is 28.
#' @param na.rm Logical. Should NA values be removed during calculations? Default is TRUE.
#' @param inf.replace Numeric value to replace INF/-INF values. Default is NA.
#' @param zero.replace Numeric value to replace division by zero results. Default is NA.
#' @return A object of the same class as \code{price} or a vector (if
#' \code{try.xts} fails) containing the VHF values with NA/INF values handled.
#' @note If Close prices are given, the function calculates the max/min using
#' only those prices. If HLC prices are given, the function uses high/low prices.
#' @author Deng Yishuo
#' @seealso See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{ADX}},
#' \code{\link{TDI}}, \code{\link{GMMA}} for other trend indicators.
#' @references \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=119}
#' @keywords ts
#' @export
#' @examples
#' data(TSLA)
#' vhf_close <- VHF(TSLA[, "Close"], na.rm = TRUE)
#' vhf_hlc <- VHF(TSLA[, c("High", "Low", "Close")], inf.replace = 10)
VHF <- function(price, n = 28, na.rm = TRUE, inf.replace = NA, zero.replace = NA) {
  # Convert input to xts or matrix
  price <- try.xts(price, error = as.matrix)

  # Validate input dimensions
  if (NCOL(price) != 1 && NCOL(price) != 3) {
    stop("Price series must be either Close, or High-Low-Close")
  }

  # Extract price components
  if (NCOL(price) == 1) {
    high <- low <- close <- price
  } else {
    high <- price[, 1]
    low <- price[, 2]
    close <- price[, 3]
  }

  # Handle NA values in input
  if (na.rm) {
    high <- na.omit(high)
    low <- na.omit(low)
    close <- na.omit(close)
  }

  # Calculate highest high and lowest low with NA handling
  hmax <- runMax(high, n)
  lmin <- runMin(low, n)

  # Calculate price changes and sum with NA handling
  price_diff <- abs(momentum(close, n = 1, na.pad = TRUE))
  denom <- runSum(price_diff, n)

  # Handle division by zero cases
  zero_mask <- denom == 0
  if (any(zero_mask, na.rm = TRUE)) {
    denom[zero_mask] <- NA
  }

  # Calculate VHF and handle INF/NA values
  VHF <- (hmax - lmin) / denom

  # Replace INF values
  if (!is.na(inf.replace)) {
    VHF[is.infinite(VHF)] <- inf.replace
  }

  # Replace division by zero results
  if (!is.na(zero.replace)) {
    VHF[zero_mask] <- zero.replace
  }

  # Preserve original class and index
  reclass(VHF, price)
}
