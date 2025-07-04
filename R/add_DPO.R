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
#' @title Calculate De - Trended Price Oscillator
#' @description
#' The Detrended Price Oscillator (DPO) removes the trend in prices - or other
#' series - by subtracting a moving average of the price from the price. The
#' Detrended Price shows cycles and overbought / oversold conditions.
#' @param OHLCV Price, volume, etc. series that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Number of periods for moving average. Defaults to 10.
#' @param maType A function or a string naming the function to be called.
#' @param shift The number of periods to shift the moving average. Defaults to n / 2 + 1.
#' @param percent logical; if \code{TRUE}, the percentage difference between the
#' slow and fast moving averages is returned, otherwise the difference between
#' the respective averages is returned. Defaults to FALSE.
#' @param append A logical value. If \code{TRUE}, the calculated De - Trended Price Oscillator
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' De - Trended Price Oscillator values will be returned. Defaults to \code{FALSE}.
#' @param ... Other arguments to be passed to the \code{maType} function.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the DPO values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated De - Trended Price Oscillator values appended, maintaining the integrity of the time - series
#' alignment.
#' @note
#' DPO does not extend to the last date because it is based on a displaced moving
#' average. The calculation shifts the results \code{shift} periods, so the last
#' \code{shift} periods will be zero.\cr
#' As stated above, the DPO can be used on any univariate series, not just price.
#' @section Warning: The detrended price oscillator removes the trend in the
#' series by centering the moving average. Centering the moving average causes it
#' to include future data. Therefore, even though this indicator looks like a
#' classic oscillator, it should not be used for trading rule signals.
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.  See \code{\link{MACD}} for a general
#' oscillator.
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:detrended_price_osci}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' priceDPO_result1 <- add_DPO(TSLA)
#'
#' # Modifying n and without appending
#' priceDPO_result2 <- add_DPO(TSLA, n = 15)
#'
#' # Using default parameters and appending
#' priceDPO_result3 <- add_DPO(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' priceDPO_result4 <- add_DPO(TSLA, n = 15, append = TRUE)
#' }
add_DPO <- function(OHLCV, n = 10, maType, shift = n / 2 + 1, percent = FALSE, append = FALSE, ...) {
  # Assume we use Close price for calculation, can be adjusted
  x <- OHLCV[, "Close"]

  x <- try.xts(x, error = as.matrix)

  ma_args <- list(n = n, ...)
  # Default MA
  if (missing(maType)) {
    maType <- "SMA"
  }

  mavg <- do.call(maType, c(list(x), ma_args))
  mavg <- lag.xts(mavg, -shift)

  if (percent) {
    dpo <- 100 * (x[, 1] / mavg - 1)
  } else {
    dpo <- x[, 1] - mavg
  }

  dpo <- reclass(dpo, x)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, dpo)
    return(combined_result)
  } else {
    return(dpo)
  }
}
