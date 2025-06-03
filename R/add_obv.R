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

#' Calculate and add On-Balance Volume (OBV) to stock data
#' @description This function calculates OBV, a technical analysis momentum indicator
#' that uses volume flow to predict changes in stock price.
#' @param mktdata xts object containing OHLCV data downloaded via quantmod
#' @param append logical. If TRUE, add OBV column to mktdata; otherwise, return only OBV column. Default is TRUE.
#' @return
#' If \code{append} is \code{TRUE}, a tibble object with added OBV column.
#' If \code{FALSE}, a tibble object with only OBV column.
#' @note Input data must contain closing prices (Close) and volume (Volume)
#' @details
#' OBV is calculated by:
#' 1. If today's closing price is higher than yesterday's, then:
#'    OBV = Previous OBV + Today's Volume
#' 2. If today's closing price is lower than yesterday's, then:
#'    OBV = Previous OBV - Today's Volume
#' 3. If today's closing price equals yesterday's, then:
#'    OBV = Previous OBV (no change)
#' @keywords OBV, On-Balance Volume, technical analysis, quantmod
#' @importFrom tibble tibble
#' @importFrom quantmod Cl Vo
#' @importFrom dplyr bind_cols
#' @export
#' @examples
#' library(quantmod)
#' library(tibble)
#' library(dplyr)
#' # Load internal Tesla stock data
#' data(TSLA)
#' # Calculate and add OBV column to TSLA data
#' tsla_with_obv <- add_obv(TSLA)
#' head(tsla_with_obv)
#' # Calculate OBV and return only OBV column
#' tsla_obv <- add_obv(TSLA, append = FALSE)
#' head(tsla_obv)
add_obv <- function(mktdata, append = TRUE) {
  # Validate input data format
  if (!inherits(mktdata, "xts")) {
    stop("mktdata must be an xts object")
  }

  # Check for required columns (Close and Volume)
  required_cols <- c("Close", "Volume")
  if (!all(required_cols %in% colnames(mktdata))) {
    stop("mktdata must contain 'Close' and 'Volume' columns")
  }

  # Extract closing prices and volume
  close_prices <- Cl(mktdata)
  volume <- Vo(mktdata)

  # Initialize OBV vector with NA
  obv <- rep(NA, length(close_prices))

  # Set first value of OBV to the first volume value
  obv[1] <- as.numeric(volume[1])

  # Calculate OBV for subsequent periods
  for (i in 2:length(close_prices)) {
    if (close_prices[i] > close_prices[i - 1]) {
      obv[i] <- obv[i - 1] + as.numeric(volume[i])
    } else if (close_prices[i] < close_prices[i - 1]) {
      obv[i] <- obv[i - 1] - as.numeric(volume[i])
    } else {
      obv[i] <- obv[i - 1]
    }
  }

  # Convert OBV to a tibble
  obv_tibble <- tibble(obv = obv)

  # Return results based on append parameter
  if (append) {
    mktdata_tibble <- tibble::as_tibble(mktdata)
    result_table <- bind_cols(mktdata_tibble, obv_tibble)
  } else {
    result_table <- obv_tibble
  }

  return(result_table)
}
