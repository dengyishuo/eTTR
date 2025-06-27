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
#' @title Aroon
#' @description
#' The Aroon indicator attempts to identify starting trends. The indicator
#' consists of up and down lines, which measure how long it has been since the
#' highest high / lowest low has occurred in the last \code{n} periods. The Aroon
#' indicator is developed by Tushar Chande in 1995.
#' @details
#' Aroon up (down) is the elapsed time, expressed as a percentage, between today
#' and the highest (lowest) price in the last \code{n} periods. If today's
#' price is a new high (low) Aroon up (down) will be 100. Each subsequent period
#' without another new high (low) causes Aroon up (down) to decrease by \eqn{(1/
#' n*100}.
#' @aliases add_Aroon
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' @param n Number of periods to use in the calculation. Defaults to 20.
#' @param append A logical value. If \code{TRUE}, the calculated result columns
#' (aroonUp, aroonDn, oscillator) will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' result data will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' (or a matrix if \code{try.xts} fails) containing the columns:
#' \describe{
#'   \item{aroonUp}{The Aroon up indicator.}
#'   \item{aroonDn}{The Aroon down indicator.}
#'   \item{oscillator}{The Aroon oscillator (\code{aroonUp - aroonDn}).}
#' }
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated columns appended, maintaining the integrity of the time - series
#' alignment.
#' @note If High - Low prices are given, the function calculates the max/min using
#' the high/low prices. Otherwise the function calculates the max/min of the
#' single series.
#' Up (down) trends are indicated when the aroonUp(Dn) is between 70 and 100.
#' Strong trends are indicated when when the aroonUp(Dn) is above 70 while the
#' aroonDn(Up) is below 30. Also, crossovers may be useful.
#' @author DengYishuo
#' @seealso See \code{\link{CCI}}, \code{\link{ADX}}, \code{\link{TDI}},
#' \code{\link{VHF}}, \code{\link{GMMA}} for other indicators that measure trend
#' direction/strength.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.fmlabs.com/reference/Aroon.htm}\cr
#' \url{https://www.fmlabs.com/reference/AroonOscillator.htm}\cr
#' \url{https://www.linnsoft.com/techind/aroon - arn}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:aroon}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' # Load required data, assume TSLA is data in the appropriate format
#' data(TSLA)
#'
#' # Use default parameters
#' aroon_result1 <- add_aroon(TSLA)
#'
#' # Modify the n parameter
#' aroon_result2 <- add_aroon(TSLA, n = 30)
#'
#' # Set append to TRUE
#' aroon_result3 <- add_aroon(TSLA, append = TRUE)
#'
#' # Combine modifying the n parameter and setting append to TRUE
#' aroon_result4 <- add_aroon(TSLA, n = 30, append = TRUE)
#' }
add_aroon <- function(OHLCV, n = 20, append = FALSE) {
  # 从OHLCV中提取HL数据
  hl <- OHLCV[, c("High", "Low")]

  hl <- try.xts(hl, error = as.matrix)
  # Calculation if price vector is given
  if (NCOL(hl) == 1) {
    high <- hl
    low <- hl
  } else
  # Calculation if HL series is given
  if (NCOL(hl) == 2) {
    high <- hl[, 1]
    low <- hl[, 2]
  } else {
    stop("Price series must be either High - Low, or Close")
  }
  # Calculate Aroon UP and DOWN
  aroonUp <- .Call(aroon_max, high, n)
  aroonDn <- .Call(aroon_max, -low, n)
  oscillator <- aroonUp - aroonDn
  result <- cbind(aroonUp, aroonDn, oscillator)
  colnames(result) <- c("aroonUp", "aroonDn", "oscillator")
  result <- reclass(result, hl)

  if (append) {
    OHLCV <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(OHLCV, result)
    return(combined_result)
  } else {
    return(result)
  }
}
