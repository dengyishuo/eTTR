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

#' @title Elastic Volume-Weighted Moving Average (EVWMA)
#' @description
#' Calculate a volume-weighted moving average that adjusts to trading volume.
#'
#' @param price Price series that is coercible to xts or matrix.
#' @param volume Volume series (must match length of \code{price} or be constant).
#' @param n Number of periods to average over.
#' @return An object of the same class as \code{price} containing the EVWMA values.
#' @keywords ts
#' @useDynLib eTTR, .registration = TRUE
#' @export
#' @examples
#' data(TSLA)
#' evwma_20 <- EVWMA(TSLA[, "Close"], TSLA[, "Volume"], 20)
#' head(evwma_20)
EVWMA <- function(price, volume, n = 10) {
  # Elastic, Volume-Weighted Moving Average
  price <- try.xts(price, error = as.matrix)
  volume <- try.xts(volume, error = as.matrix)

  if (!any(NROW(volume) == c(NROW(price), 1))) {
    stop("Length of 'volume' must equal 1 or the length of 'price'")
  }
  if (n < 1 || n > NROW(price)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(price)))
  }
  if (NCOL(price) > 1 || NCOL(volume) > 1) {
    stop("EVWMA only supports univariate 'price' and 'volume'")
  }

  pv <- cbind(price, volume)
  if (any(n > colSums(!is.na(pv)))) {
    stop("n > number of non-NA values in price/volume")
  }

  # Call C routine (assuming .Call is defined)
  ma <- .Call(C_evwma, pv[, 1], pv[, 2], n)
  ma <- reclass(ma, price)

  if (!is.null(dim(ma))) {
    colnames(ma) <- "EVWMA"
  }

  return(ma)
}
