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
#' @title Arnaud Legoux Moving Average (ALMA)
#'
#' @description Calculate a Gaussian-weighted moving average with reduced lag.
#'
#' @param x Price series that is coercible to xts or matrix.
#' @param n Number of periods to average over.
#' @param offset Percentile for weight distribution center (0-1).
#' @param sigma Standard deviation of the Gaussian distribution.
#' @return An object of the same class as \code{x} containing the ALMA values.
#' @note Higher \code{offset} emphasizes recent prices; lower \code{sigma} reduces smoothing.
#' @keywords ts
#' @export
#' @examples
#' data(TSLA)
#' tsla_alma_9 <- ALMA(TSLA[, "Close"], 9)
#' head(tsla_alma_9)
ALMA <- function(x, n = 9, offset = 0.85, sigma = 6) {
  # Arnaud Legoux Moving Average
  x <- try.xts(x, error = as.matrix)

  if (offset < 0 || offset > 1) {
    stop("Please ensure 0 <= offset <= 1")
  }
  if (sigma <= 0) {
    stop("sigma must be > 0")
  }

  m <- floor(offset * (n - 1))
  s <- n / sigma
  wts <- exp(-((seq(0, n - 1) - m)^2) / (2 * s * s))
  sumWeights <- sum(wts)
  if (sumWeights != 0) {
    wts <- wts / sumWeights
  }

  alma <- x * NA_real_
  for (i in seq_len(NCOL(x))) {
    alma[, i] <- WMA(x[, i], n, wts)
  }

  if (!is.null(dim(alma))) {
    colnames(alma) <- "ALMA"
  }

  return(reclass(alma, x))
}
