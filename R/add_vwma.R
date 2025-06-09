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
#' @title Calculate and add Volume Weighted Moving Averages (VWMA) to stock data
#' @description This function calculates VWMA, which weights each price by its corresponding
#' trading volume, giving more importance to price movements that occur during high volume periods.
#' @param mktdata xts object containing OHLCV data downloaded via quantmod
#' @param periods numeric vector with 2-4 positive integers representing VWMA periods
#' @param append logical. If TRUE, add VWMA columns to mktdata; otherwise, return only VWMA columns. Default is TRUE.
#' @return
#' If \code{append} is \code{TRUE}, a tibble object with added VWMA columns.
#' If \code{FALSE}, a tibble object with only VWMA columns.
#' @note Input data must contain closing prices (Close) and volume (Volume)
#' @details
#' VWMA is calculated by summing the product of each price and its volume over a specified period,
#' then dividing by the total volume over that period.
#' @keywords VWMA, volume weighted, technical analysis, quantmod
#' @importFrom tibble tibble
#' @importFrom quantmod Cl Vo
#' @importFrom zoo rollapply
#' @importFrom dplyr bind_cols
#' @export
#' @examples
#' # Load internal Tesla stock data
#' data(TSLA)
#' # Calculate and add VWMA columns to TSLA data
#' tsla_with_vwma <- add_vwma(TSLA, periods = c(10, 20), append = TRUE)
#' head(tsla_with_vwma)
#' # Return only VWMA columns for TSLA
#' tsla_only_vwma <- add_vwma(TSLA, periods = c(10, 20), append = FALSE)
#' head(tsla_only_vwma)
add_vwma <- function(mktdata, periods = c(50, 200), append = TRUE) {
  # Validate input data format
  if (!inherits(mktdata, "xts")) {
    stop("mktdata must be an xts object")
  }

  # Check for required columns (Close and Volume)
  required_cols <- c("Close", "Volume")
  if (!all(required_cols %in% colnames(mktdata))) {
    stop("mktdata must contain 'Close' and 'Volume' columns")
  }

  # Validate periods input
  stopifnot(
    "Periods must be 2-4 positive integers" = all(periods > 0) & length(periods) %in% 2:4
  )

  # Extract closing prices and volume
  close_prices <- Cl(mktdata)
  volume <- Vo(mktdata)

  # Calculate VWMA for each period
  vwma_list <- lapply(periods, function(n) {
    # Calculate volume-weighted price (Close * Volume)
    vwap <- close_prices * volume

    # Calculate rolling sum of volume-weighted price
    sum_vwap <- rollapply(vwap, width = n, FUN = sum, align = "right", fill = NA)

    # Calculate rolling sum of volume
    sum_volume <- rollapply(volume, width = n, FUN = sum, align = "right", fill = NA)

    # Compute VWMA by dividing sum of vwap by sum of volume
    vwma_values <- sum_vwap / sum_volume
    coredata(vwma_values)
  })

  # Convert results to data.frame and then tibble
  vwma_df <- data.frame(vwma_list)
  colnames(vwma_df) <- paste0("vwma_", periods)
  vwma_tibble <- tibble::as_tibble(vwma_df)

  # Return results based on append parameter
  if (append) {
    mktdata_tibble <- tibble::as_tibble(mktdata)
    result_table <- bind_cols(mktdata_tibble, vwma_tibble)
  } else {
    result_table <- vwma_tibble
  }

  return(result_table)
}
