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
#' @title Calculate Arms' Ease of Movement Value
#' @description
#' Arms' Ease of Movement Value (EMV) emphasizes days where the security moves
#' easily and minimizes days where the security does not move easily. Developed
#' by Richard W. Arms, Jr.
#' The EMV is calculated by dividing the midpoint \code{[high + low\]/2} move by
#' the 'Box Ratio' (volume divided by the high minus low).
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume prices.
#' @param n Number of periods for moving average. Defaults to 9.
#' @param maType A function or a string naming the function to be called.
#' @param vol.divisor An increment to make the results larger and easier to work
#' with. Defaults to 10000.
#' @param append A logical value. If \code{TRUE}, the calculated Arms' Ease of Movement Value
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Arms' Ease of Movement Value values will be returned. Defaults to \code{FALSE}.
#' @param ... Other arguments to be passed to the \code{maType} function.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a matrix (if \code{try.xts} fails) containing the columns:
#'  \describe{
#'   \item{ emv }{ The ease of movement values. }
#'   \item{ maEMV }{ The smoothed (as specified by \code{ma}) ease of movement values. }
#'  }
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Arms' Ease of Movement Value values appended, maintaining the integrity of the time - series
#' alignment.
#' @note A buy/sell signal is generated when the EMV crosses above/below zero.
#' When the EMV hovers around zero, there are small price movements and/or high
#' volume, and the price is not moving easily.
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.fmlabs.com/reference/ArmsEMV.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=51}\cr
#' \url{https://www.linnsoft.com/techind/arms - ease - movement}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' emv_result1 <- add_EMV(TSLA)
#'
#' # Modifying n and without appending
#' emv_result2 <- add_EMV(TSLA, n = 12)
#'
#' # Using default parameters and appending
#' emv_result3 <- add_EMV(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' emv_result4 <- add_EMV(TSLA, n = 12, append = TRUE)
#' }
add_EMV <- function(OHLCV, n = 9, maType, vol.divisor = 10000, append = FALSE, ...) {
  # Extract HL and volume from OHLCV
  hl <- OHLCV[, c("High", "Low")]
  volume <- OHLCV[, "Volume"]

  if (missing(hl) || missing(volume)) {
    stop("High - Low matrix (HL) and volume vector must be specified.")
  }

  hl <- try.xts(hl, error = as.matrix)
  volume <- try.xts(volume, error = as.matrix)

  if (!(is.xts(hl) && is.xts(volume))) {
    hl <- as.matrix(hl)
    volume <- as.matrix(volume)
  }

  mid <- (hl[, 1] + hl[, 2]) / 2
  volume <- volume / vol.divisor

  emv <- momentum(mid, n = 1, na.pad = TRUE) / (volume / (hl[, 1] - hl[, 2]))

  ma_args <- list(n = n, ...)
  # Default MA
  if (missing(maType)) {
    maType <- "SMA"
  }

  maEMV <- do.call(maType, c(list(emv), ma_args))

  result <- cbind(emv, maEMV)
  colnames(result) <- c("emv", "maEMV")

  result <- reclass(result, hl)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, result)
    return(combined_result)
  } else {
    return(result)
  }
}
