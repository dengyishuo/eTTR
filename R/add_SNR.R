#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2016  Peter Carl, Joshua M. Ulrich
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
#' @title Calculate Signal to Noise Ratio
#' @description
#' The n - day SNR for a given market is calculated by taking the absolute
#' price change over an n - day period and dividing it by the average
#' n - day volatility.
#'
#' \deqn{SNR_n = \frac{|C_t - C_{t - n}|}{ATR_n}
#' }{SNR = abs(Cl - lag(Cl,n)) / ATR(HLC, n)$atr}
#'
#' Using average true range as the volatility measure captures more of the
#' intraday and overnight volatility in a way that a measurement of
#' Close - to - Close price change does not.
#'
#' The interpretation is then relatively intuitive: an SNR value of five
#' indicates that the market has moved five times the volatility (average true
#' range) over the given look - back period.
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Number of periods for moving average.
#' @param append A logical value. If \code{TRUE}, the calculated Signal to Noise Ratio
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Signal to Noise Ratio values will be returned. Defaults to \code{FALSE}.
#' @param ... Other arguments to be passed to \code{\link{ATR}}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a matrix (if \code{try.xts} fails) containing the signal to noise ratio.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Signal to Noise Ratio values appended, maintaining the integrity of the time - series
#' alignment.
#' @author Peter Carl
#' @references Skeggs, James and Hill, Alex (2015). Back in Black Part 2: The
#' Opportunity Set for Trend Following.
#' @export
add_SNR <- function(OHLCV, n, append = FALSE, ...) {
  # Assume we use High - Low - Close prices for calculation, can be adjusted
  hlc <- OHLCV[, c("High", "Low", "Close")]
  hlc <- try.xts(hlc, error = as.matrix)

  # Calculate SNR
  # abs(HLC[, 3] - lag.xts(HLC[, 3], n)) gives the absolute price change over an n - day period
  # ATR(HLC, n, ...)[, "atr"] gives the average n - day volatility (using Average True Range)
  snr <- abs(hlc[, 3] - lag.xts(hlc[, 3], n)) / ATR(hlc, n, ...)[, "atr"]

  snr <- reclass(snr, hlc)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, snr)
    return(combined_result)
  } else {
    return(snr)
  }
}
