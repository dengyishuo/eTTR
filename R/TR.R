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

#' True Range Calculation
#'
#' Computes the True Range (TR) of a financial instrument based on its High-Low-Close prices.
#' The True Range is the greatest of the following:
#' 1. Current high minus current low
#' 2. Absolute value of current high minus previous close
#' 3. Absolute value of current low minus previous close
#'
#' @title True Range
#' @description Calculates the True Range of a price series.
#'
#' @param HLC Object that is coercible to xts or matrix and contains High-Low-Close prices.
#'
#' @return A object of the same class as \code{HLC} or a matrix (if \code{try.xts} fails)
#'         containing the columns:
#'         \describe{
#'           \item{tr}{The true range of the series.}
#'           \item{trueHigh}{The true high used in calculating the true range.}
#'           \item{trueLow}{The true low used in calculating the true range.}
#'         }
#'
#' @author DengYishuo
#' @keywords ts
#'
#' @examples
#' data(ttrc)
#' tr <- TR(ttrc[, c("High", "Low", "Close")])
#'
#' @seealso \code{\link{ATR}} for Average True Range.
#'
#' @importFrom xts try.xts lag.xts reclass
#'
#' @export
TR <- function(HLC) {
  # Convert input to xts or matrix
  HLC <- try.xts(HLC, error = as.matrix)

  # Calculate previous close
  if (is.xts(HLC)) {
    closeLag <- lag.xts(HLC[, 3])
  } else {
    closeLag <- c(NA, HLC[-NROW(HLC), 3])
  }

  # Compute true high, true low, and true range
  trueHigh <- pmax(HLC[, 1], closeLag, na.rm = FALSE)
  trueLow <- pmin(HLC[, 2], closeLag, na.rm = FALSE)
  tr <- trueHigh - trueLow

  # Combine results
  result <- cbind(tr, trueHigh, trueLow)
  colnames(result) <- c("tr", "trueHigh", "trueLow")

  # Preserve original class
  reclass(result, HLC)
}
