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
#' @title Zig Zag
#' @description
#' Zig Zag higlights trends by removing price changes smaller than \code{change}
#' and interpolating lines between the extreme points.
#'
#' The Zig Zag is non-predictive.  The purpose of the Zig Zag is filter noise
#' and make chart patterns clearer.  It's more a visual tool than an indicator.
#' @aliases ZigZag zigzag
#' @param HL Object that is coercible to xts or matrix and contains either a
#' High-Low price series, or a Close price series.
#' @param change Minimum price movement, either in dollars or percent (see
#' \code{percent}).
#' @param percent Use percentage or dollar change?
#' @param retrace Is \code{change} a retracement of the previous move, or an
#' absolute change from peak to trough?
#' @param lastExtreme If the extreme price is the same over multiple periods,
#' should the extreme price be the first or last observation?
#' @return A object of the same class as \code{HL} or a vector (if
#' \code{try.xts} fails) containing the Zig Zag indicator.
#' @note If High-Low prices are given, the function calculates the max/min using
#' the high/low prices.  Otherwise the function calculates the max/min of the
#' single series.
#' @section Warning: The last value of the ZigZag indicator is unstable (i.e.
#' unknown) until the turning point actually occurs. Therefore this indicator
#' isn't well-suited for use for systematic trading strategies.
#' @author DengYishuo
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://www.fmlabs.com/reference/default.htm?url=ZigZag.htm}\cr
#' \url{https://www.linnsoft.com/techind/zig-zag-indicator-zig-zzo}\cr
#' \url{https://www.linnsoft.com/techind/zig-zag-oscillator-indicator-zzo}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=127}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:zigzag}\cr
#' @keywords ts
#' @export
#' @examples
#' ## Get Data and Indicator
#' data(TSLA)
#' zz <- ZigZag(TSLA[, c("High", "Low")], change = 20)
ZigZag <-
  function(HL, change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE) {
    # Zig Zag Indicator
    # Adapted from Alberto Santini's code

    HL <- try.xts(HL, error = as.matrix)
    HL.na <- naCheck(HL, 0)

    # Calculation if HL series is given
    if (NCOL(HL) == 2) {
      high <- HL[HL.na$nonNA, 1]
      low <- HL[HL.na$nonNA, 2]
    } else

    # Calculation if price vector is given
    if (NCOL(HL.na) == 1) {
      high <- HL[HL.na$nonNA]
      low <- HL[HL.na$nonNA]
    } else {
      stop("Price series must be either High-Low, or Univariate")
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
    zz <- c(rep(NA, HL.na$NAs), zz)

    reclass(zz, HL)
  }
