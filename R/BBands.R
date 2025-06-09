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
#' @title Bollinger Bands
#' @description
#' Calculates Bollinger Bands, a volatility indicator comparing price levels over time.
#' @param HLC Object coercible to xts/matrix containing High-Low-Close prices.
#' Univariate series are accepted.
#' @param n Number of periods for moving average (default 20).
#' @param maType Function or string naming moving average type.
#' @param sd Number of standard deviations (default 2).
#' @param ... Additional arguments passed to `maType`.
#' @return
#' Object matching `HLC` class with columns:
#' - `dn`: Lower band
#' - `mavg`: Moving average
#' - `up`: Upper band
#' - `pctB`: %B value
#' @note
#' Non-SMA averages cause inconsistencies since SD calculations assume SMA.
#' @details
#' Calculates three bands:
#' - Middle: SMA of typical price \eqn{(high + low + close)/3}
#' - Upper: `sd` standard deviations above MA
#' - Lower: `sd` standard deviations below MA
#' Uses univariate series directly if provided.
#' @references
#' - \url{https://www.fmlabs.com/reference/Bollinger.htm}
#' - \url{https://school.stockcharts.com/doku.php?id=technical_indicators:bollinger_bands}
#' @seealso
#' Moving average functions: [eTTR::SMA()], [eTTR::EMA()]
#' @keywords ts
#' @importFrom xts xcoredata
#' @export
#' @examples
#' data(TSLA)
#' # Using HLC series
#' bbands_hlc <- BBands(TSLA[, c("High", "Low", "Close")])
#' # Using Close only
#' bbands_close <- BBands(TSLA[, "Close"])
BBands <- function(HLC, n = 20, maType, sd = 2, ...) {
  # Original function implementation remains unchanged
  HLC <- try.xts(HLC, error = as.matrix)
  if (NCOL(HLC) == 3) {
    if (is.xts(HLC)) {
      xa <- xts::xcoredata(HLC)
      HLC <- xts(apply(HLC, 1, mean), index(HLC))
      xts::xcoredata(HLC) <- xa
    } else {
      HLC <- apply(HLC, 1, mean)
    }
  } else if (NCOL(HLC) != 1) {
    stop("Price series must be either High-Low-Close, or Close/univariate.")
  }

  maArgs <- list(n = n, ...)
  if (missing(maType)) {
    maType <- "SMA"
  }

  mavg <- do.call(maType, c(list(HLC), maArgs))
  sdev <- runSD(HLC, n, sample = FALSE)

  up <- mavg + sd * sdev
  dn <- mavg - sd * sdev
  pctB <- (HLC - dn) / (up - dn)

  res <- cbind(dn, mavg, up, pctB)
  colnames(res) <- c("dn", "mavg", "up", "pctB")

  reclass(res, HLC)
}
