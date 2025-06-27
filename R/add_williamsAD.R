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
#' @title Calculate Williams Accumulation / Distribution
#' @description
#' The Williams Accumulation / Distribution (AD) line is a measure of market
#' momentum. Developed by Larry Williams.
#' The Williams AD line differs from OBV and chaikinAD in that it doesn't take
#' volume into account.
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param append A logical value. If \code{TRUE}, the calculated Williams AD values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Williams AD values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the accumulation / distribution values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Williams AD values appended, maintaining the integrity of the time - series
#' alignment.
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
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' ad_result1 <- add_williamsAD(TSLA)
#'
#' # Using default parameters and appending
#' ad_result2 <- add_williamsAD(TSLA, append = TRUE)
#' }
add_williamsAD <- function(OHLCV, append = FALSE) {
  # Assume we use High - Low - Close prices for calculation, can be adjusted
  HLC <- OHLCV[, c("High", "Low", "Close")]
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

  ad <- reclass(ad, HLC)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, ad)
    return(combined_result)
  } else {
    return(ad)
  }
}
