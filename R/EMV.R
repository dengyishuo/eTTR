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

#' @title Arms' Ease of Movement Value
#' @description
#' Arms' Ease of Movement Value (EMV) emphasizes days where the security moves
#' easily and minimizes days where the security does not move easily.  Developed
#' by Richard W. Arms, Jr.
#' The EMV is calculated by dividing the midpoint \code{[high + low\]/2} move by
#' the 'Box Ratio' (volume divided by the high minus low).
#' @param HL Object that is coercible to xts or matrix and contains High-Low
#' prices.
#' @param volume Vector or matrix of volume observations corresponding to the
#' \code{HL} object.
#' @param n Number of periods for moving average.
#' @param maType A function or a string naming the function to be called.
#' @param vol.divisor An increment to make the results larger and easier to work
#' with.
#' @param \dots Other arguments to be passed to the \code{maType} function.
#' @return A object of the same class as \code{HL} and \code{volume} or a matrix
#' (if \code{try.xts} fails) containing the columns:
#'  \describe{
#'   \item{ emv }{ The ease of movement values. }
#'   \item{ maEMV }{ The smoothed (as specified by \code{ma}) ease of movement values. }
#'  }
#' @note A buy/sell signal is generated when the EMV crosses above/below zero.
#' When the EMV hovers around zero, there are small price movements and/or high
#' volume, and the price is not moving easily.
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.fmlabs.com/reference/ArmsEMV.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=51}\cr
#' \url{https://www.linnsoft.com/techind/arms-ease-movement}\cr
#' @keywords ts
#' @export
#' @examples
#' data(TSLA)
#' emv <- EMV(TSLA[, c("High", "Low")], TSLA[, "Volume"])
EMV <-
  function(HL, volume, n = 9, maType, vol.divisor = 10000, ...) {
    # Arms' Ease of Movement Value

    if (missing(HL) || missing(volume)) {
      stop("High-Low matrix (HL) and volume vector must be specified.")
    }

    HL <- try.xts(HL, error = as.matrix)
    volume <- try.xts(volume, error = as.matrix)

    if (!(is.xts(HL) && is.xts(volume))) {
      HL <- as.matrix(HL)
      volume <- as.matrix(volume)
    }

    mid <- (HL[, 1] + HL[, 2]) / 2
    volume <- volume / vol.divisor

    emv <- momentum(mid, n = 1, na.pad = TRUE) / (volume / (HL[, 1] - HL[, 2]))

    maArgs <- list(n = n, ...)
    # Default MA
    if (missing(maType)) {
      maType <- "SMA"
    }

    maEMV <- do.call(maType, c(list(emv), maArgs))

    result <- cbind(emv, maEMV)
    colnames(result) <- c("emv", "maEMV")

    reclass(result, HL)
  }
