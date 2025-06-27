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
#' @title Calculate Chaikin Money Flow
#' @description
#' Chaikin Money Flow compares total volume over the last \code{n} time periods
#' to total volume times the Close Location Value (CLV) over the last \code{n}
#' time periods. Developed by Marc Chaikin.
#' Chaikin Money Flow is calculated by taking dividing the sum of the Chaikin
#' Accumulation / Distribution line over the past \code{n} periods by the sum of
#' volume over the past \code{n} periods.
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume prices.
#' @param n Number of periods to use. Defaults to 20.
#' @param append A logical value. If \code{TRUE}, the calculated Chaikin Money Flow
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Chaikin Money Flow values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the Chaikin Money Flow values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Chaikin Money Flow values appended, maintaining the integrity of the time - series
#' alignment.
#' @note When Chaikin Money Flow is above/below +/- 0.25 it is a bullish/bearish
#' signal. If Chaikin Money Flow remains below zero while the price is rising,
#' it indicates a probable reversal.
#' @author DengYishuo
#' @seealso See \code{\link{CLV}}, and \code{\link{chaikinAD}}.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.fmlabs.com/reference/ChaikinMoneyFlow.htm}\cr
#' \url{https://www.linnsoft.com/techind/chaikin - money - flow - cmf}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:chaikin_money_flow_cmf}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' cmf_result1 <- add_CMF(TSLA)
#'
#' # Modifying n and without appending
#' cmf_result2 <- add_CMF(TSLA, n = 30)
#'
#' # Using default parameters and appending
#' cmf_result3 <- add_CMF(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' cmf_result4 <- add_CMF(TSLA, n = 30, append = TRUE)
#' }
add_CMF <- function(OHLCV, n = 20, append = FALSE) {
  # Extract HLC and volume from OHLCV
  hlc <- OHLCV[, c("High", "Low", "Close")]
  volume <- OHLCV[, "Volume"]

  hlc <- try.xts(hlc, error = as.matrix)
  volume <- try.xts(volume, error = as.matrix)

  if (!(is.xts(hlc) && is.xts(volume))) {
    clv <- CLV(as.matrix(hlc))
    volume <- as.matrix(volume)
  }
  clv <- CLV(hlc)

  cmf <- runSum(clv * volume, n) / runSum(volume, n)

  cmf <- reclass(cmf, hlc)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, cmf)
    return(combined_result)
  } else {
    return(cmf)
  }
}
