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
#' @title Calculate Parabolic Stop - and - Reverse
#' @description
#' The Parabolic Stop - and - Reverse calculates a trailing stop. Developed by J.
#' Welles Wilder.
#' The calculation for the SAR is quite complex. See the URLs in the references
#' section for calculation notes.
#' The SAR assumes that you are always in the market, and calculates the Stop
#' And Reverse point when you would close a long position and open a short
#' position or vice versa.
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param accel A vector where \code{accel[1]} is the initial acceleration factor
#' and \code{accel[2]} is the maximum acceleration factor. Defaults to c(.02, .2).
#' @param append A logical value. If \code{TRUE}, the calculated Parabolic Stop - and - Reverse
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Parabolic Stop - and - Reverse values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the Parabolic Stop and Reverse values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Parabolic Stop and Reverse values appended, maintaining the integrity of the time - series
#' alignment.
#' @export
#' @author DengYishuo
#' @seealso See \code{\link{ATR}} and \code{\link{ADX}}, which were also
#' developed by Welles Wilder.
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://www.linnsoft.com/techind/parabolic - sar - sar}\cr
#' \url{https://www.fmlabs.com/reference/SAR.htm}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:parabolic_sar}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=87}
#' @keywords ts
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' sar_result1 <- add_SAR(TSLA)
#'
#' # Modifying accel and without appending
#' sar_result2 <- add_SAR(TSLA, accel = c(0.03, 0.25))
#'
#' # Using default parameters and appending
#' sar_result3 <- add_SAR(TSLA, append = TRUE)
#'
#' # Modifying accel and appending
#' sar_result4 <- add_SAR(TSLA, accel = c(0.03, 0.25), append = TRUE)
#' }
add_SAR <- function(OHLCV, accel = c(.02, .2), append = FALSE) {
  # Assume we use High - Low prices for calculation, can be adjusted
  HL <- OHLCV[, c("High", "Low")]
  HL <- try.xts(HL, error = as.matrix)

  # Check for non - leading NAs
  # Leading NAs are handled in the C code
  naCheck(HL, 0) # called for error handling side - effect

  # Call C routine
  sar <- .Call(sar, HL[, 1], HL[, 2], accel)
  colnames(sar) <- "sar"

  sar <- reclass(sar, HL)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, sar)
    return(combined_result)
  } else {
    return(sar)
  }
}
