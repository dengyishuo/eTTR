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

#' Calculate and add Exponential Volume Weighted Moving Averages (EVWMA) to stock data
#' @description This function calculates EVWMA, which weights recent prices more heavily
#' and incorporates trading volume, making it responsive to price movements during high volume periods.
#' @param mktdata xts object containing OHLCV data downloaded via quantmod
#' @param periods numeric vector with 2-4 positive integers representing EVWMA periods
#' @param append logical. If TRUE, add EVWMA columns to mktdata; otherwise, return only EVWMA columns. Default is TRUE.
#' @return
#' If \code{append} is \code{TRUE}, a tibble object with added EVWMA columns.
#' If \code{FALSE}, a tibble object with only EVWMA columns.
#' @note Input data must contain closing prices (Close) and volume (Volume)
#' @details
#' EVWMA is calculated by applying an exponential moving average to the volume-weighted price.
#' This gives more importance to price movements that occur with higher trading volume.
#' @keywords EVWMA, volume weighted, technical analysis, quantmod, TTR
#' @importFrom tibble tibble
#' @importFrom quantmod Cl Vo
#' @importFrom dplyr bind_cols
#' @export
#' @examples
#' library(quantmod)
#' library(tibble)
#' library(TTR)
#' library(dplyr)
#' # Load internal Tesla stock data
#' data(TSLA)
#' # Calculate and add EVWMA columns to TSLA data
#' tsla_with_evwma <- add_evwma(TSLA, periods = c(10, 20), append = TRUE)
#' head(tsla_with_evwma)
#' # Return only EVWMA columns for TSLA
#' tsla_only_evwma <- add_evwma(TSLA, periods = c(10, 20), append = FALSE)
#' head(tsla_only_evwma)
add_evwma <- function(mktdata, periods = c(50, 200), append = TRUE) {
  # Validate input data format
  if (!inherits(mktdata, "xts")) {
    stop("mktdata must be an xts object")
  }

  # Check for required columns (Close and Volume)
  required_cols <- c("Close", "Volume")
  if (!all(required_cols %in% colnames(mktdata))) {
    stop("mktdata must contain 'Close' and 'Volume' columns")
  }

  # Load TTR package if not available
  if (!requireNamespace("TTR", quietly = TRUE)) {
    install.packages("TTR")
    library(TTR)
  }

  # Validate periods input
  stopifnot(
    "Periods must be 2-4 positive integers" = all(periods > 0) & length(periods) %in% 2:4
  )

  # Extract closing prices and volume
  close_prices <- Cl(mktdata)
  volume <- Vo(mktdata)

  # Calculate volume-weighted price
  vwap <- close_prices * volume

  # Calculate EVWMA for each period
  evwma_list <- lapply(periods, function(n) {
    # Calculate EMA of volume-weighted price
    ema_vwap <- EMA(vwap, n = n)
    # Calculate EMA of volume
    ema_volume <- EMA(volume, n = n)
    # Compute EVWMA by dividing EMA of vwap by EMA of volume
    evwma_values <- ema_vwap / ema_volume
    coredata(evwma_values)
  })

  # Convert results to data.frame and then tibble
  evwma_df <- data.frame(evwma_list)
  colnames(evwma_df) <- paste0("evwma_", periods)
  evwma_tibble <- tibble::as_tibble(evwma_df)

  # Return results based on append parameter
  if (append) {
    mktdata_tibble <- tibble::as_tibble(mktdata)
    result_table <- bind_cols(mktdata_tibble, evwma_tibble)
  } else {
    result_table <- evwma_tibble
  }

  return(result_table)
}
