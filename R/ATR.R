# eTTR: Enhanced Technical Trading Rules
#
# Copyright (C) 2025-2030  DengYishuo
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#' @title Calculate Average True Range (ATR)
#' @description
#' This function computes the Average True Range (ATR), a technical analysis indicator
#' used to measure market volatility. It calculates the true range (TR) and applies a moving
#' average (defaulting to Wilder's EMA) to smooth the results.
#' @param HLC An OHLC (Open-High-Low-Close) price series, typically an xts object.
#' @param n The number of periods to use for the moving average calculation. Defaults to 14.
#' @param maType The type of moving average to use. Defaults to "EMA" (Wilder's EMA).
#' @param ... Additional arguments to be passed to the moving average function.
#' @return An xts object containing columns for the true range ("tr"), the ATR ("atr"),
#'         and the true high and low values ("trueHigh", "trueLow").
#' @details
#' The Average True Range (ATR) is a measure of volatility introduced by J. Welles Wilder.
#' It considers the entire range of price movement in a given period, including gaps between
#' sessions. The default method uses Wilder's EMA, which applies a smoothing factor of 1/n.
#' @references
#' Wilder, J. Welles. "New Concepts in Technical Trading Systems." 1978.
#' @author DengYishuo
#' @importFrom xts xts reclass
#' @importFrom zoo coredata
#' @examples
#' \dontrun{
#' # Calculate ATR for a stock's OHLC data
#' data(sample_matrix)
#' stock_ohlc <- as.xts(sample_matrix)
#' atr_values <- ATR(stock_ohlc, n = 14)
#' }
#' @export
ATR <- function(HLC, n = 14, maType, ...) {
  HLC <- try.xts(HLC, error = as.matrix)
  tr <- TR(HLC)
  maArgs <- list(n = n, ...)
  if (missing(maType)) {
    maType <- "EMA"
    if (is.null(maArgs$wilder)) {
      maArgs$wilder <- TRUE
    }
  }
  atr <- do.call(maType, c(list(tr[, 1]), maArgs))
  result <- cbind(tr[, 1], atr, tr[, 2:3])
  colnames(result) <- c("tr", "atr", "trueHigh", "trueLow")
  reclass(result, HLC)
}
