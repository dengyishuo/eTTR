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

#' @title Chaikin Accumulation / Distribution
#' @description
#' The Chaikin Accumulation / Distribution (AD) line is a measure of the money
#' flowing into or out of a security.  It is similar to On Balance Volume (OBV).
#' Developed by Marc Chaikin.
#'
#' The AD line is similar to OBV; the difference is that OBV sums volume
#' multiplied by +/- 1 if the close is higher/lower than the previous close,
#' while the AD line multiplies volume by the close location value (CLV).
#'
#' @param HLC Object that is coercible to xts or matrix and contains
#' High-Low-Close prices.
#' @param volume Vector or matrix of volume observations corresponding to the
#' \code{HLC} object.
#' @return A object of the same class as \code{HLC} and \code{volume} or a
#' vector (if \code{try.xts} fails) containing the accumulation / distribution
#' values.
#' @note The Accumulation/Distribution Line is interpreted by looking for a
#' divergence in the direction of the indicator relative to price.
#' @author DengYishuo
#' @seealso See \code{\link{OBV}}, and \code{\link{CLV}}.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.fmlabs.com/reference/AccumDist.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=27}\cr
#' \url{https://www.linnsoft.com/techind/accumulation-distribution}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:accumulation_distribution_line}\cr
#' @export
#' @keywords ts
#' @examples
#' data(TSLA)
#' ad <- chaikinAD(TSLA[, c("High", "Low", "Close")], TSLA[, "Volume"])
chaikinAD <-
  function(HLC, volume) {
    # Chaikin Accumulation / Distribution

    HLC <- try.xts(HLC, error = as.matrix)
    volume <- try.xts(volume, error = as.matrix)

    if (!(is.xts(HLC) && is.xts(volume))) {
      HLC <- as.matrix(HLC)
      volume <- as.matrix(volume)
    }

    ad <- CLV(HLC) * volume

    ad.na <- naCheck(ad)
    ad <- cumsum(ad[ad.na$nonNA])
    ad <- c(rep(NA, ad.na$NAs), ad)

    reclass(ad, HLC)
  }
