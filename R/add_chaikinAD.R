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
#' @title Calculate Chaikin Accumulation / Distribution
#' @description
#' The Chaikin Accumulation / Distribution (AD) line is a measure of the money
#' flowing into or out of a security.  It is similar to On Balance Volume (OBV).
#' Developed by Marc Chaikin.
#'
#' The AD line is similar to OBV; the difference is that OBV sums volume
#' multiplied by +/- 1 if the close is higher/lower than the previous close,
#' while the AD line multiplies volume by the close location value (CLV).
#' @param OHLCV Object that is coercible to xts or matrix and contains
#' Open - High - Low - Close - Volume prices.
#' @param append A logical value. If \code{TRUE}, the calculated accumulation / distribution
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' accumulation / distribution values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the accumulation / distribution
#' values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated accumulation / distribution values appended, maintaining the integrity of the time - series
#' alignment.
#' @note The Accumulation/Distribution Line is interpreted by looking for a
#' divergence in the direction of the indicator relative to price.
#' @author DengYishuo
#' @seealso See \code{\link{OBV}}, and \code{\link{CLV}}.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.fmlabs.com/reference/AccumDist.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=27}\cr
#' \url{https://www.linnsoft.com/techind/accumulation - distribution}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:accumulation_distribution_line}\cr
#' @export
#' @keywords ts
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' ad_result1 <- add_chaikinAD(TSLA)
#'
#' # Using default parameters and appending
#' ad_result2 <- add_chaikinAD(TSLA, append = TRUE)
#' }
add_chaikinAD <- function(OHLCV, append = FALSE) {
  # Extract HLC and volume from OHLCV
  hlc <- OHLCV[, c("High", "Low", "Close")]
  volume <- OHLCV[, "Volume"]

  hlc <- try.xts(hlc, error = as.matrix)
  volume <- try.xts(volume, error = as.matrix)

  if (!(is.xts(hlc) && is.xts(volume))) {
    hlc <- as.matrix(hlc)
    volume <- as.matrix(volume)
  }

  ad <- CLV(hlc) * volume

  ad_na <- naCheck(ad)
  ad <- cumsum(ad[ad_na$nonNA])
  ad <- c(rep(NA, ad_na$NAs), ad)

  ad <- reclass(ad, hlc)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, ad)
    return(combined_result)
  } else {
    return(ad)
  }
}
