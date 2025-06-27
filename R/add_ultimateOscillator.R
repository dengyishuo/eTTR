#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2025 - 2030  DengYishuo
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
#' @title Calculate The Ultimate Oscillator
#' @description
#' The Ultimate Oscillator is a momentum oscillator designed to capture momentum across three
#' different time frames.
#' Created by Larry Williams in 1976.
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n A vector of the number of periods to use for each average calculation. Default is c(7, 14, 28).
#' @param wts The weights applied to each average. Default is c(4, 2, 1).
#' @param append A logical value. If \code{TRUE}, the calculated Ultimate Oscillator
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Ultimate Oscillator values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a matrix (if \code{try.xts} fails) containing the Ultimate Oscillator values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Ultimate Oscillator values appended, maintaining the integrity of the time - series
#' alignment.
#' @author Ivan Popivanov
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:ultimate_oscillator}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' ult_osc_result1 <- add_ultimateOscillator(TSLA)
#'
#' # Modifying n and without appending
#' ult_osc_result2 <- add_ultimateOscillator(TSLA, n = c(5, 10, 20))
#'
#' # Using default parameters and appending
#' ult_osc_result3 <- add_ultimateOscillator(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' ult_osc_result4 <- add_ultimateOscillator(TSLA, n = c(5, 10, 20), append = TRUE)
#' }
add_ultimateOscillator <- function(OHLCV, n = c(7, 14, 28), wts = c(4, 2, 1), append = FALSE) {
  if (length(n) != 3 || length(wts) != 3) {
    stop("length(n) and length(wts) must both be 3")
  }

  # Assume we use High - Low - Close prices for calculation, can be adjusted
  HLC <- OHLCV[, c("High", "Low", "Close")]
  HLC <- try.xts(HLC, error = as.matrix)

  # avoid reclassing in ATR and runSum
  HLC_RECLASS <- attr(HLC, ".RECLASS")
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
  attr(HLC, ".RECLASS") <- HLC_RECLASS

  osc <- reclass(osc, HLC)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, osc)
    return(combined_result)
  } else {
    return(osc)
  }
}
