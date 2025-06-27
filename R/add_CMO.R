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
#' @title Calculate Chande Momentum Oscillator
#' @description
#' The Chande Momentum Oscillator (CMO) is a modified RSI. Developed by Tushar
#' S. Chande.
#' The CMO divides the total movement by the net movement ((up - down) / (up + down)),
#' where RSI divides the upward movement by the net movement (up / (up + down)).
#' @param OHLCV Price, volume, etc. series that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Number of periods to use. Defaults to 14.
#' @param append A logical value. If \code{TRUE}, the calculated Chande Momentum Oscillator
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Chande Momentum Oscillator values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing Chande Momentum Oscillator values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Chande Momentum Oscillator values appended, maintaining the integrity of the time - series
#' alignment.
#' @note There are several ways to interpret the CMO:
#'  \enumerate{
#'    \item Values over/under +/- 50 indicate overbought/oversold conditions.
#'    \item High CMO values indicate strong trends.
#'    \item When the CMO crosses above/below a moving average of the CMO,
#'          it is a buy/sell signal.
#'  }
#' @author DengYishuo
#' @seealso See \code{\link{RSI}}.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.fmlabs.com/reference/CMO.htm}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' cmo_result1 <- add_CMO(TSLA)
#'
#' # Modifying n and without appending
#' cmo_result2 <- add_CMO(TSLA, n = 20)
#'
#' # Using default parameters and appending
#' cmo_result3 <- add_CMO(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' cmo_result4 <- add_CMO(TSLA, n = 20, append = TRUE)
#' }
add_CMO <- function(OHLCV, n = 14, append = FALSE) {
  # Assume we use Close price for calculation, can be adjusted
  x <- OHLCV[, "Close"]

  x <- try.xts(x, error = as.matrix)

  up <- momentum(x, n = 1)
  dn <- ifelse(up < 0, abs(up), 0)
  up <- ifelse(up > 0, up, 0)

  up <- runSum(up, n)
  dn <- runSum(dn, n)

  cmo <- 100 * (up - dn) / (up + dn)

  if (!is.null(dim(cmo)) && ncol(cmo) == 1L) {
    colnames(cmo) <- "cmo"
  }

  cmo <- reclass(cmo, x)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, cmo)
    return(combined_result)
  } else {
    return(cmo)
  }
}
