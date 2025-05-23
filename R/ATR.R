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

#' True Range / Average True Range
#'
#' True range (TR) is a measure of volatility of a High-Low-Close series;
#' average true range (ATR) is a Welles Wilder's style moving average of the TR.
#' Developed by J. Welles Wilder in 1978.
#'
#' TR incorporates yesterday's close in the calculation (high minus low).  E.g.
#' if yesterday's close was higher than today's high, then the TR would equal
#' yesterday's close minus today's low.
#'
#' The ATR is a component of the Welles Wilder Directional Movement Index
#' (\code{DX}, \code{ADX}).
#'
#' @aliases ATR TR
#' @param HLC Object that is coercible to xts or matrix and contains
#' High-Low-Close prices.
#' @param n Number of periods for moving average.
#' @param maType A function or a string naming the function to be called.
#' @param \dots Other arguments to be passed to the \code{maType} function.
#' @return A object of the same class as \code{HLC} or a matrix (if
#' \code{try.xts} fails) containing the columns:
#'  \describe{
#'   \item{ tr }{ The true range of the series. }
#'   \item{ atr }{ The average (as specified by \code{ma}) true range of the series. }
#'   \item{ trueHigh }{ The true high of the series. }
#'   \item{ trueLow }{ The true low of the series. }
#'  }
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.  See \code{\link{DX}}, which uses true
#' range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.fmlabs.com/reference/TR.htm}\cr
#' \url{https://www.fmlabs.com/reference/ATR.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=35}\cr
#' \url{https://www.linnsoft.com/techind/true-range-tr}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:average_true_range_atr}\cr
#' @keywords ts
#' @examples
#'
#' data(ttrc)
#' tr <- TR(ttrc[, c("High", "Low", "Close")])
#' atr <- ATR(ttrc[, c("High", "Low", "Close")], n = 14)
#'
#' @rdname ATR
"TR" <-
  function(HLC) {
    # True Range

    HLC <- try.xts(HLC, error = as.matrix)

    if (is.xts(HLC)) {
      closeLag <- lag.xts(HLC[, 3])
    } else {
      closeLag <- c(NA, HLC[-NROW(HLC), 3])
    }

    trueHigh <- pmax(HLC[, 1], closeLag, na.rm = FALSE)
    trueLow <- pmin(HLC[, 2], closeLag, na.rm = FALSE)
    tr <- trueHigh - trueLow

    result <- cbind(tr, trueHigh, trueLow)
    colnames(result) <- c("tr", "trueHigh", "trueLow")

    reclass(result, HLC)
  }

#' @rdname ATR
"ATR" <-
  function(HLC, n = 14, maType, ...) {
    # Average True Range / True Range

    HLC <- try.xts(HLC, error = as.matrix)
    tr <- TR(HLC)

    maArgs <- list(n = n, ...)

    # Default Welles Wilder EMA
    if (missing(maType)) {
      maType <- "EMA"
      if (is.null(maArgs$wilder)) {
        # do not overwrite user-provided value
        maArgs$wilder <- TRUE
      }
    }

    atr <- do.call(maType, c(list(tr[, 1]), maArgs))

    result <- cbind(tr[, 1], atr, tr[, 2:3])
    colnames(result) <- c("tr", "atr", "trueHigh", "trueLow")

    reclass(result, HLC)
  }
