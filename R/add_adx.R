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
#' @title Welles Wilder's Directional Movement Index
#' @description Directional Movement Index; developed by J. Welles Wilder.
#' The \code{DIp}/\code{DIn} (positive/negative) is the percentage of the true
#' range that is up/down. This function calculates the Directional Movement Index
#' and related components.
#' @aliases add_ADX
#' @param OHLCV Object that is coercible to \code{xts} or \code{matrix} and contains
#' Open - High - Low - Close - Volume (and potentially other) prices. It serves as
#' the input data source for the calculations.
#' @param n Number of periods to use for DX calculation (not ADX calculation).
#' Defaults to 14.
#' @param maType A function or a string naming the function to be called. This is
#' used to specify the type of moving average for calculating the ADX.
#' @param append A logical value. If \code{TRUE}, the calculated result columns
#' (DIp, DIn, DX, ADX) will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' result data will be returned. Defaults to \code{FALSE}
#' @param ... Other arguments to be passed to the \code{maType} function.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' (or a matrix if \code{try.xts} fails) containing the columns:
#' \describe{
#'   \item{DIp}{The positive Direction Index.}
#'   \item{DIn}{The negative Direction Index.}
#'   \item{DX}{The Direction Index.}
#'   \item{ADX}{The Average Direction Index (trend strength).}
#' }
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated columns appended, maintaining the integrity of the time - series
#' alignment.
#' @note A buy/sell signal is generated when the +/-DI crosses up over the
#' -/+DI, when the DX/ADX signals a strong trend. A high/low DX signals a
#' strong/weak trend. DX is usually smoothed with a moving average (i.e. the
#' ADX).
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section. The DX calculation uses
#' \code{\link{ATR}}. See \code{\link{aroon}}, \code{\link{CCI}},
#' \code{\link{TDI}}, \code{\link{VHF}}, \code{\link{GMMA}} for other indicators
#' that measure trend direction/strength.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.fmlabs.com/reference/DI.htm}\cr
#' \url{https://www.fmlabs.com/reference/DX.htm}\cr
#' \url{https://www.fmlabs.com/reference/ADX.htm}\cr
#' \url{https://www.fmlabs.com/reference/ADXR.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=49}\cr
#' \url{https://www.linnsoft.com/techind/directional-indicator-diplus-diminus}\cr
#' \url{https://www.linnsoft.com/techind/adx-avg-directional-movement}\cr
#' \url{https://www.linnsoft.com/techind/adxr-avg-directional-movement-rating}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:average_directional_index_adx}\cr
#' @export
#' @keywords ts
#' @examples
#' \dontrun{
#' # Load required data, assume TSLA is data in OHLCV format
#' data(TSLA)
#'
#' # Use default parameters
#' dmi.adx1 <- add_adx(TSLA)
#'
#' # Modify the n parameter
#' dmi.adx2 <- add_adx(TSLA, n = 20)
#'
#' # Use SMA as maType
#' dmi.adx3 <- add_adx(TSLA, maType = "SMA")
#'
#' # Use SMA and pass additional parameters to the SMA function
#' dmi.adx4 <- add_adx(TSLA, maType = "SMA")
#'
#' # Set append to TRUE
#' dmi.adx5 <- add_adx(TSLA, append = TRUE)
#'
#' # Combine modifying the n parameter and setting append to TRUE
#' dmi.adx6 <- add_adx(TSLA, n = 20, append = TRUE)
#'
#' # Combine modifying the n parameter, using SMA as maType, and setting append to TRUE
#' dmi.adx7 <- add_adx(TSLA, n = 20, maType = "SMA", append = TRUE)
#' }
add_adx <- function(OHLCV, n = 14, maType, append = FALSE, ...) {
  # Welles Wilder's Directional Movement Index
  hlc <- HLC(OHLCV)
  hlc <- try.xts(hlc, error = as.matrix)
  dH <- momentum(hlc[, 1])
  dL <- -momentum(hlc[, 2])

  DMIp <- ifelse(dH == dL | (dH < 0 & dL < 0), 0, ifelse(dH > dL, dH, 0))
  DMIn <- ifelse(dH == dL | (dH < 0 & dL < 0), 0, ifelse(dH < dL, dL, 0))

  tr <- TR(hlc)[, "tr"]
  TRsum <- wilderSum(tr, n = n)

  DIp <- 100 * wilderSum(DMIp, n = n) / TRsum
  DIn <- 100 * wilderSum(DMIn, n = n) / TRsum

  DX <- 100 * (abs(DIp - DIn) / (DIp + DIn))

  maArgs <- list(n = n, ...)

  # Default Welles Wilder EMA
  if (missing(maType)) {
    maType <- "EMA"
    if (is.null(maArgs$wilder)) {
      # do not overwrite user - provided value
      maArgs$wilder <- TRUE
    }
  }

  ADX <- do.call(maType, c(list(DX), maArgs))

  result <- cbind(DIp, DIn, DX, ADX)
  colnames(result) <- c("DIp", "DIn", "DX", "ADX")
  result <- reclass(result, HLC)

  if (append) {
    OHLCV <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(OHLCV, result)
    return(combined_result)
  } else {
    return(result)
  }
}
