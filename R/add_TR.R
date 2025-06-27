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
#' @title Calculate True Range
#' @description Calculates the True Range of a price series.
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param append A logical value. If \code{TRUE}, the calculated True Range, True High, and True Low
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' True Range, True High, and True Low values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a matrix (if \code{try.xts} fails) containing the columns:
#'         \describe{
#'           \item{tr}{The true range of the series.}
#'           \item{trueHigh}{The true high used in calculating the true range.}
#'           \item{trueLow}{The true low used in calculating the true range.}
#'         }
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated True Range, True High, and True Low values appended, maintaining the integrity of the time - series
#' alignment.
#' @seealso \code{\link{ATR}} for Average True Range.
#' @importFrom xts try.xts lag.xts reclass
#' @author DengYishuo
#' @keywords ts
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' tr_result1 <- add_TR(TSLA)
#'
#' # Using default parameters and appending
#' tr_result2 <- add_TR(TSLA, append = TRUE)
#' }
#' @export
add_TR <- function(OHLCV, append = FALSE) {
  # Assume we use High - Low - Close prices for calculation, can be adjusted
  HLC <- OHLCV[, c("High", "Low", "Close")]
  # Convert input to xts or matrix
  HLC <- try.xts(HLC, error = as.matrix)

  # Calculate previous close
  if (is.xts(HLC)) {
    # If HLC is an xts object, use lag.xts to get the previous close
    closeLag <- lag.xts(HLC[, 3])
  } else {
    # If HLC is a matrix, create a vector with NA as the first element and the rest as previous close values
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
  result <- reclass(result, HLC)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, result)
    return(combined_result)
  } else {
    return(result)
  }
}
