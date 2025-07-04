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
#' @title The Ultimate Oscillator
#' @description
#' The Ultimate Oscillator is a momentum oscillator designed to capture momentum across three
#' different time frames.
#'
#' Created by Larry Williams in 1976.
#'
#' @param HLC Object that is coercible to xts or matrix and contains
#' High-Low-Close prices.
#' @param n A vector of the number of periods to use for each average calculation.
#' @param wts The weights applied to each average.
#' @author Ivan Popivanov
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:ultimate_oscillator}\cr
#' @keywords ts
#' @export
#' @examples
#' data(TSLA)
#' ult.osc <- ultimateOscillator(TSLA[, c("High", "Low", "Close")])
ultimateOscillator <-
  function(HLC, n = c(7, 14, 28), wts = c(4, 2, 1)) {
    # Ultimate Oscillator

    if (length(n) != 3 || length(wts) != 3) {
      stop("length(n) and length(wts) must both be 3")
    }

    HLC <- try.xts(HLC, error = as.matrix)

    # avoid reclassing in ATR and runSum
    HLC.RECLASS <- attr(HLC, ".RECLASS")
    attr(HLC, ".RECLASS") <- FALSE

    # only need 'tr' and 'trueLow'
    atr <- ATR(HLC, n = 1)

    buyPressure <- HLC[, 3] - atr[, "trueLow"]

    osc <- buyPressure * 0.0
    for (i in 1:3) {
      osc <- osc + wts[i] * (runSum(buyPressure, n[i]) / runSum(atr[, "tr"], n[i]))
    }
    osc <- 100.0 * osc / sum(wts)

    # restore HLC .RECLASS attribute
    attr(HLC, ".RECLASS") <- HLC.RECLASS

    reclass(osc, HLC)
  }
