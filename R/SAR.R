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

#' @title Parabolic Stop-and-Reverse
#' @description
#' The Parabolic Stop-and-Reverse calculates a trailing stop.  Developed by J.
#' Welles Wilder.
#'
#' The calculation for the SAR is quite complex.  See the URLs in the references
#' section for calculation notes.
#'
#' The SAR assumes that you are always in the market, and calculates the Stop
#' And Reverse point when you would close a long position and open a short
#' position or vice versa.
#'
#' @param HL Object that is coercible to xts or matrix and contains High-Low
#' prices.
#' @param accel \code{accel[1]}: Acceleration factor.\cr \code{accel[2]}: Maximum acceleration
#' factor.
#' @return A object of the same class as \code{HL} or a vector (if
#' \code{try.xts} fails) containing the Parabolic Stop and Reverse values.
#' @export
#' @author DengYishuo
#' @seealso See \code{\link{ATR}} and \code{\link{ADX}}, which were also
#' developed by Welles Wilder.
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://www.linnsoft.com/techind/parabolic-sar-sar}\cr
#' \url{https://www.fmlabs.com/reference/SAR.htm}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:parabolic_sar}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=87}
#' @keywords ts
#' @examples
#' data(TSLA)
#' sar <- SAR(TSLA[, c("High", "Low")])
SAR <-
  function(HL, accel = c(.02, .2)) {
    # Parabolic Stop-and-Reverse (SAR)
    # ----------------------------------------------
    #       HL = HL vector, matrix, or dataframe
    # accel[1] = acceleration factor
    # accel[2] = maximum acceleration factor

    # WISHLIST:
    # Determine signal based on DM+/DM- for first bar
    # If sig[1]==1, then ep[1]==high; if sig[1]==-1, then ep[1]==low
    # The first SAR value should be the opposite (high/low) of ep
    # The first acceleration factor is based on the first signal

    # Since I've already lost one bar, do what TA-lib does and use that bar to
    # determine the inital signal value.  Also try to incorporate different
    # accel factors for long/short.
    # accel = c( long = c( 0.02, 0.2 ), short = long )

    HL <- try.xts(HL, error = as.matrix)

    # Check for non-leading NAs
    # Leading NAs are handled in the C code
    naCheck(HL, 0) # called for error handling side-effect

    # Call C routine
    sar <- .Call(sar, HL[, 1], HL[, 2], accel)
    colnames(sar) <- "sar"

    reclass(sar, HL)
  }
