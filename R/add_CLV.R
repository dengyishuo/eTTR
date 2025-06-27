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
#' @title Calculate Close Location Value
#' @description
#' The Close Location Value (CLV) relates the day's close to its trading range.
#' The CLV will fall in a range of -1 to +1.  If the CLV is +/-1, the close is
#' at the high/low; if the CLV is 0, the close is directly between the high and
#' low.
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume prices.
#' @param append A logical value. If \code{TRUE}, the calculated Close Location Values
#' will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Close Location Values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the Close Location Values of a
#' High - Low - Close price series.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Close Location Values appended, maintaining the integrity of the time - series
#' alignment.
#' @author DengYishuo
#' @seealso See \code{\link{chaikinAD}}, which uses CLV.
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:accumulation_distribution_line}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' clv_result1 <- add_CLV(TSLA)
#'
#' # Using default parameters and appending
#' clv_result2 <- add_CLV(TSLA, append = TRUE)
#' }
add_CLV <- function(OHLCV, append = FALSE) {
  # Extract HLC from OHLCV
  hlc <- OHLCV[, c("High", "Low", "Close")]

  hlc <- try.xts(hlc, error = as.matrix)
  clv <- ((hlc[, 3] - hlc[, 2]) - (hlc[, 1] - hlc[, 3])) / (hlc[, 1] - hlc[, 2])

  # Account for H = L = C
  clv[is.nan(clv) | is.infinite(clv)] <- 0

  if (is.xts(clv)) colnames(clv) <- "clv"
  clv <- reclass(clv, hlc)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, clv)
    return(combined_result)
  } else {
    return(clv)
  }
}
