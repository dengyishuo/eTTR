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

#' @title Chaikin Volatility
#' @description
#'
#' Chaikin Volatility measures the rate of change of the security's trading
#' range.  Developed by Marc Chaikin.
#'
#' The Chaikin Volatility indicator defines volatility as an increase in the
#' difference between the high and low.
#'
#' @param HL Object that is coercible to xts or matrix and contains High-Low
#' prices.
#' @param n Number of periods for moving average.
#' @param maType A function or a string naming the function to be called.
#' @param \dots Other arguments to be passed to the \code{maType} function.
#' @return A object of the same class as \code{HL} or a vector (if
#' \code{try.xts} fails) containing the Chaikin Volatility values.
#' @note A rapid increase in Chaikin Volatility indicates an approaching bottom.
#' A slow decrease in Chaikin Volatility indicates an approaching top.
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.  See \code{\link{TR}} for another
#' volatility measure.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.fmlabs.com/reference/ChaikinVolatility.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=120}\cr
#' @export
#' @keywords ts
#' @examples
#' data(TSLA)
#' volatility <- chaikinVolatility(TSLA[, c("High", "Low")])
chaikinVolatility <-
  function(HL, n = 10, maType, ...) {
    # Chaikin Volatility

    HL <- try.xts(HL, error = as.matrix)

    maArgs <- list(n = n, ...)
    # Default MA
    if (missing(maType)) {
      maType <- "EMA"
    }

    mavg <- do.call(maType, c(list(HL[, 1] - HL[, 2]), maArgs))

    volatility <- ROC(mavg, n, type = "discrete")

    reclass(volatility, HL)
  }
