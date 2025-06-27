# eTTR: Enhanced Technical Trading Rules
#
# Copyright (C) 2025 - 2030  DengYishuo
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
#' @param OHLCV An OHLCV (Open - High - Low - Close - Volume) price series, typically an xts object.
#' @param n The number of periods to use for the moving average calculation. Defaults to 14.
#' @param maType The type of moving average to use. Defaults to "EMA" (Wilder's EMA).
#' @param append A logical value. If \code{TRUE}, the calculated result columns
#' ("tr", "atr", "trueHigh", "trueLow") will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' result data will be returned. Defaults to \code{FALSE}.
#' @param ... Additional arguments to be passed to the moving average function.
#' @return If \code{append = FALSE}, an xts object containing columns for the true range ("tr"), the ATR ("atr"),
#'         and the true high and low values ("trueHigh", "trueLow").
#'         If \code{append = TRUE}, an xts object of the same class as \code{OHLCV} with the
#'         calculated columns appended, maintaining the integrity of the time - series
#'         alignment.
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
#' # Calculate ATR for a stock's OHLCV data
#' data(TSLA)
#' atr_values <- add_ATR(TSLA, n = 14)
#'
#' # Using default parameters and appending results
#' atr_append1 <- add_ATR(TSLA, append = TRUE)
#'
#' # Modifying n and appending results
#' atr_append2 <- add_ATR(TSLA, n = 20, append = TRUE)
#' }
#' @export
add_ATR <- function(OHLCV, n = 14, maType, append = FALSE, ...) {
  # extract OHLC from  OHLCV
  ohlc <- OHLCV[, c("Open", "High", "Low", "Close")]

  ohlc <- try.xts(ohlc, error = as.matrix)
  tr <- TR(ohlc)
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
  result <- reclass(result, ohlc)

  if (append) {
    OHLCV <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(OHLCV, result)
    return(combined_result)
  } else {
    return(result)
  }
}
