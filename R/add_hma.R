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

#' Calculate and add Hull Moving Averages (HMA) to stock data
#' @description This function calculates HMA, a highly efficient moving average
#' that reduces lag significantly while maintaining smoothness, making it ideal for
#' identifying trends and reversals.
#' @param mktdata xts object containing OHLCV data downloaded via quantmod
#' @param periods numeric vector with 2-4 positive integers representing HMA periods
#' @param append logical. If TRUE, add HMA columns to mktdata; otherwise, return only HMA columns. Default is TRUE.
#' @return
#' If \code{append} is \code{TRUE}, a tibble object with added HMA columns.
#' If \code{FALSE}, a tibble object with only HMA columns.
#' @note Input data must contain closing prices (Close)
#' @details
#' HMA is calculated using a three-step process:
#' 1. Compute a WMA with period n
#' 2. Compute a WMA with period n/2
#' 3. Compute a WMA of the difference between the two previous WMAs with period sqrt(n)
#' @keywords HMA, Hull Moving Average, technical analysis, quantmod, TTR
#' @importFrom tibble tibble
#' @importFrom TTR WMA
#' @importFrom quantmod Cl
#' @importFrom dplyr bind_cols
#' @export
#' @examples
#' library(quantmod)
#' library(tibble)
#' library(TTR)
#' library(dplyr)
#' # Load internal Tesla stock data
#' data(TSLA)
#' # Calculate and add HMA columns to TSLA data
#' tsla_with_hma <- add_hma(TSLA, periods = c(10, 20), append = TRUE)
#' head(tsla_with_hma)
#' # Return only HMA columns for TSLA
#' tsla_only_hma <- add_hma(TSLA, periods = c(10, 20), append = FALSE)
#' head(tsla_only_hma)
add_hma <- function(mktdata, periods = c(50, 200), append = TRUE) {
  # Validate input data format
  if (!inherits(mktdata, "xts")) {
    stop("mktdata must be an xts object")
  }

  # Check for required columns (Close)
  required_cols <- c("Close")
  if (!all(required_cols %in% colnames(mktdata))) {
    stop("mktdata must contain 'Close' column")
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

  # Extract closing prices
  close_prices <- Cl(mktdata)

  # Calculate HMA for each period
  hma_list <- lapply(periods, function(n) {
    # Calculate WMA of full period
    wma_n <- WMA(close_prices, n = n)

    # Calculate WMA of half period (rounded down)
    half_n <- floor(n / 2)
    wma_half_n <- WMA(close_prices, n = half_n)

    # Calculate difference series
    diff_series <- 2 * wma_half_n - wma_n

    # Calculate square root of period (rounded)
    sqrt_n <- round(sqrt(n))

    # Calculate final HMA using square root period
    hma_values <- WMA(diff_series, n = sqrt_n)
    coredata(hma_values)
  })

  # Convert results to data.frame and then tibble
  hma_df <- data.frame(hma_list)
  colnames(hma_df) <- paste0("hma_", periods)
  hma_tibble <- tibble::as_tibble(hma_df)

  # Return results based on append parameter
  if (append) {
    mktdata_tibble <- tibble::as_tibble(mktdata)
    result_table <- bind_cols(mktdata_tibble, hma_tibble)
  } else {
    result_table <- hma_tibble
  }

  return(result_table)
}
