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
#' @title Calculate Zig Zag
#' @description
#' Zig Zag higlights trends by removing price changes smaller than \code{change}
#' and interpolating lines between the extreme points.
#'
#' The Zig Zag is non - predictive. The purpose of the Zig Zag is filter noise
#' and make chart patterns clearer. It's more a visual tool than an indicator.
#' @aliases add_zigzag
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain
#' Open - High - Low - Close - Volume data.
#' @param change Minimum price movement, either in dollars or percent (see
#' \code{percent}).
#' @param percent Use percentage or dollar change?
#' @param retrace Is \code{change} a retracement of the previous move, or an
#' absolute change from peak to trough?
#' @param lastExtreme If the extreme price is the same over multiple periods,
#' should the extreme price be the first or last observation?
#' @param append A logical value. If \code{TRUE}, the calculated Zig Zag values
#' will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Zig Zag values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the Zig Zag indicator.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Zig Zag values appended, maintaining the integrity of the time - series
#' alignment.
#' @note If High - Low prices are given, the function calculates the max/min using
#' the high/low prices. Otherwise the function calculates the max/min of the
#' single series.
#' @section Warning: The last value of the ZigZag indicator is unstable (i.e.
#' unknown) until the turning point actually occurs. Therefore this indicator
#' isn't well - suited for use for systematic trading strategies.
#' @author DengYishuo
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://www.fmlabs.com/reference/default.htm?url=ZigZag.htm}\cr
#' \url{https://www.linnsoft.com/techind/zig - zag - indicator - zig - zzo}\cr
#' \url{https://www.linnsoft.com/techind/zig - zag - oscillator - indicator - zzo}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=127}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:zigzag}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' # Using default parameters without appending
#' data(TSLA)
#' zz_result1 <- add_ZigZag(TSLA, change = 20)
#'
#' # Using default parameters and appending
#' zz_result2 <- add_ZigZag(TSLA, change = 20, append = TRUE)
#'
#' # Changing parameters and without appending
#' zz_result3 <- add_ZigZag(TSLA, change = 10, percent = FALSE, retrace = TRUE)
#'
#' # Changing parameters and appending
#' zz_result4 <- add_ZigZag(TSLA, change = 10, percent = FALSE, retrace = TRUE, append = TRUE)
#' }
add_ZigZag <- function(OHLCV, change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE, append = FALSE) {
  # Assume we use High - Low or Close prices for calculation, can be adjusted
  if (NCOL(OHLCV) == 1) {
    hl <- OHLCV()
  } else if (NCOL(OHLCV) >= 2) {
    hl <- OHLCV[, c("High", "Low")]
  } else {
    stop("Price series must have at least 1 (Close) or 2 (High - Low) columns")
  }

  hl <- try.xts(hl, error = as.matrix)
  hl_na <- naCheck(hl, 0)

  # Calculation if HL series is given
  if (NCOL(hl) == 2) {
    high <- hl[hl_na$nonNA, 1]
    low <- hl[hl_na$nonNA, 2]
  } else
  # Calculation if price vector is given
  if (NCOL(hl_na) == 1) {
    high <- hl[hl_na$nonNA]
    low <- hl[hl_na$nonNA]
  } else {
    stop("Price series must be either High - Low, or Univariate")
  }

  # Call C routine
  zz <- .Call(
    ettr_zigzag,
    as.numeric(high),
    as.numeric(low),
    as.numeric(change),
    as.logical(percent),
    as.logical(retrace),
    as.logical(lastExtreme)
  )

  # Interpolate results
  zz <- na.approx(zz, na.rm = FALSE)

  # Prepend NAs from original data
  zz <- c(rep(NA, hl_na$NAs), zz)

  zz <- reclass(zz, hl)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(OHLCV, zz)
    return(combined_result)
  } else {
    return(zz)
  }
}
