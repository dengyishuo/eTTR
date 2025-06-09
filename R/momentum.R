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
#' @title Momentum
#' @description
#' Calculate the momentum of a series over \code{n} periods.
#'
#' The momentum indicator provides the difference between the current value
#' and the value \code{n} periods ago.
#'
#' @param x Price, volume, etc. series that is coercible to xts or matrix.
#' @param n Number of periods to use.
#' @param na.pad Should periods prior to \code{n} be appended? Default is \code{TRUE}.
#' @return A object of the same class as \code{x} or a vector (if \code{try.xts}
#' fails) containing the momentum values.
#' @author DengYishuo
#' @keywords ts
#' @examples
#' data(TSLA)
#' mom <- momentum(TSLA[, "Close"])
#' @export
momentum <-
  function(x, n = 1, na.pad = TRUE) {
    # Momentum

    # http://www.fmlabs.com/reference/Momentum.htm
    # https://www.metastock.com/Customer/Resources/TAAZ/?p=95
    # https://www.linnsoft.com/tour/techind/momentum.htm

    x <- try.xts(x, error = as.matrix)
    if (is.xts(x)) {
      mom <- diff(x, n, na.pad = na.pad)
    } else {
      NAs <- NULL
      if (na.pad) {
        NAs <- rep(NA, n)
      }
      mom <- c(NAs, diff(x, n))
    }
    reclass(mom, x)
  }
