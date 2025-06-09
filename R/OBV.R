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
#' @title On Balance Volume (OBV)
#'
#' @description
#' On Balance Volume (OBV) is a momentum-based technical indicator that measures
#' the cumulative flow of volume to determine the strength of a price trend.
#' It uses volume flow to predict changes in stock prices by adding volume on
#' up days and subtracting volume on down days.
#'
#' @param price Price series that is coercible to xts or matrix.
#' @param volume Volume series that is coercible to xts or matrix,
#'               corresponding to the price object.
#'
#' @return A object of the same class as \code{price} and \code{volume} or a
#'         vector (if \code{try.xts} fails) containing the OBV values.
#'
#' @note OBV is typically used to confirm price trends or identify potential
#'       divergences between volume flow and price movements.
#'
#' @author DengYishuo
#'
#' @importFrom xts try.xts
#'
#' @seealso See \code{\link{chaikinAD}} for a similar volume-based indicator.
#'
#' @references
#' - https://www.fmlabs.com/reference/OBV.htm
#' - https://www.metastock.com/Customer/Resources/TAAZ/?p=82
#' - https://www.linnsoft.com/techind/balance-open-interest
#' - https://school.stockcharts.com/doku.php?id=technical_indicators:on_balance_volume_obv
#'
#' @keywords ts indicator volume momentum
#' @export
#'
#' @examples
#' data(TSLA)
#' obv <- OBV(TSLA[, "Close"], ttrc[, "Volume"])
OBV <-
  function(price, volume) {
    # On Balance Volume calculation

    price <- try.xts(price, error = as.matrix)
    volume <- try.xts(volume, error = as.matrix)

    if (!(is.xts(price) && is.xts(volume))) {
      price <- as.vector(price)
      volume <- as.vector(volume)
    }

    prChg <- ROC(price)
    obv <- c(volume[1], ifelse(prChg > 0, volume, -volume)[-1])

    # Handle zero price change cases
    obv[abs(prChg) < sqrt(.Machine$double.eps)] <- 0
    obv <- cumsum(obv)

    if (is.xts(obv)) {
      obv <- xts(obv, index(price))
      colnames(obv) <- "obv"
    }

    reclass(obv, price)
  }
