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
#' @title Calculate On Balance Volume (OBV)
#' @description
#' On Balance Volume (OBV) is a momentum - based technical indicator that measures
#' the cumulative flow of volume to determine the strength of a price trend.
#' It uses volume flow to predict changes in stock prices by adding volume on
#' up days and subtracting volume on down days.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' The function will extract the closing price and volume from this object for OBV calculation.
#' @param append A logical value. If \code{TRUE}, the calculated OBV values will be appended to the \code{OHLCV} input data,
#' ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
#' OBV values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
#' containing the OBV values.
#' If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated OBV values appended,
#' maintaining the integrity of the time - series alignment.
#' @note OBV is typically used to confirm price trends or identify potential
#'       divergences between volume flow and price movements.
#' @author DengYishuo
#' @importFrom xts try.xts
#' @seealso See \code{\link{chaikinAD}} for a similar volume - based indicator.
#' @references
#' - https://www.fmlabs.com/reference/OBV.htm
#' - https://www.metastock.com/Customer/Resources/TAAZ/?p=82
#' - https://www.linnsoft.com/techind/balance - open - interest
#' - https://school.stockcharts.com/doku.php?id=technical_indicators:on_balance_volume_obv
#' @keywords ts indicator volume momentum
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' obv_result1 <- add_OBV(TSLA)
#'
#' # Using default parameters and appending
#' obv_result2 <- add_OBV(TSLA, append = TRUE)
#' }
add_OBV <- function(OHLCV, append = FALSE) {
  # Check if OHLCV contains 'Close' and 'Volume' columns
  required_cols <- c("Close", "Volume")
  if (!all(required_cols %in% colnames(OHLCV))) {
    stop("OHLCV must contain 'Close' and 'Volume' columns")
  }

  # Extract the closing price and volume
  price <- OHLCV[, "Close"]
  volume <- OHLCV[, "Volume"]
  price <- try.xts(price, error = as.matrix)
  volume <- try.xts(volume, error = as.matrix)

  # Calculate the price change
  prChg <- ROC(price)

  # Initialize OBV
  obv <- c(volume[1], ifelse(prChg > 0, volume, -volume)[-1])

  # Handle zero price change cases
  obv[abs(prChg) < sqrt(.Machine$double.eps)] <- 0
  obv <- cumsum(obv)

  if (is.xts(obv)) {
    obv <- xts(obv, index(price))
    colnames(obv) <- "obv"
  }

  obv <- reclass(obv, price)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, obv)
    return(combined_result)
  } else {
    return(obv)
  }
}
