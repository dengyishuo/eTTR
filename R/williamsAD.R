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
#' @title Williams Accumulation / Distribution
#' @description
#' The Williams Accumulation / Distribution (AD) line is a measure of market
#' momentum.  Developed by Larry Williams.
#'
#' The Williams AD line differs from OBV and chaikinAD in that it doesn't take
#' volume into account.
#'
#' @param HLC Object that is coercible to xts or matrix and contains
#' High-Low-Close prices.
#' @return A object of the same class as \code{HLC} or a vector (if
#' \code{try.xts} fails) containing the accumulation / distribution values.
#' @note The Accumulation/Distribution Line is interpreted by looking for a
#' divergence in the direction of the indicator relative to price.
#' @author DengYishuo
#' @seealso See \code{\link{OBV}}, \code{\link{chaikinAD}}, and
#' \code{\link{ATR}}.
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://www.fmlabs.com/reference/WilliamsAD.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=125}\cr
#' @keywords ts
#' @export
#' @examples
#' data(TSLA)
#' ad <- williamsAD(TSLA[, c("High", "Low", "Close")])
williamsAD <-
  function(HLC) {
    # Williams Accumulation/Distribution

    HLC <- try.xts(HLC, error = as.matrix)

    # Calculate change in close, and true high/low
    dCl <- momentum(HLC[, 3], 1)
    atr <- ATR(HLC)

    # Calculate AD
    ad <- HLC[, 3] - ifelse(dCl > 0, atr[, "trueLow"], atr[, "trueHigh"])
    ad[dCl == 0] <- 0

    ad.na <- naCheck(ad)
    ad <- cumsum(ad[ad.na$nonNA])
    ad <- c(rep(NA, ad.na$NAs), ad)

    reclass(ad, HLC)
  }
