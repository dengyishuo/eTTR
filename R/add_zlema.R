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

#' Calculate and add Zero-Lag Exponential Moving Averages (ZLEMA) to stock data
#' @description This function calculates ZLEMA, an improved version of EMA that reduces
#' lag by incorporating past price data, making it more responsive to current price changes.
#' @param mktdata xts object containing OHLCV data downloaded via quantmod
#' @param periods numeric vector with 2-4 positive integers representing ZLEMA periods
#' @param append logical. If TRUE, add ZLEMA columns to mktdata; otherwise, return only ZLEMA columns. Default is TRUE.
#' @return
#' If \code{append} is \code{TRUE}, a tibble object with added ZLEMA columns.
#' If \code{FALSE}, a tibble object with only ZLEMA columns.
#' @note Input data must contain closing prices (Close)
#' @details
#' ZLEMA is calculated by applying an EMA to a modified price series that attempts to remove lag.
#' The modification involves adjusting the current price by subtracting a lagged price value.
#' @keywords ZLEMA, zero-lag, technical analysis, quantmod
#' @importFrom tibble tibble
#' @importFrom quantmod Cl
#' @importFrom dplyr bind_cols
#' @export
#' @examples
#' # Load internal Tesla stock data
#' data(TSLA)
#' # Calculate and add ZLEMA columns to TSLA data
#' tsla_with_zlema <- add_zlema(TSLA, periods = c(10, 20), append = TRUE)
#' head(tsla_with_zlema)
#' # Return only ZLEMA columns for TSLA
#' tsla_only_zlema <- add_zlema(TSLA, periods = c(10, 20), append = FALSE)
#' head(tsla_only_zlema)
#' @author DengYishuo
#' @family add indicator functions
add_zlema <- function(mktdata, periods = c(50, 200), append = TRUE) {
  # Validate input data format
  if (!inherits(mktdata, "xts")) {
    stop("mktdata must be an xts object")
  }

  # Check for required columns (Close)
  required_cols <- c("Close")
  if (!all(required_cols %in% colnames(mktdata))) {
    stop("mktdata must contain 'Close' column")
  }

  # Validate periods input
  stopifnot(
    "Periods must be 2-4 positive integers" = all(periods > 0) & length(periods) %in% 2:4
  )

  # Extract closing prices
  close_prices <- Cl(mktdata)

  # Calculate ZLEMA for each period
  zlema_list <- lapply(periods, function(n) {
    # Calculate lag as floor((n-1)/2)
    lag <- floor((n - 1) / 2)

    # Create lagged price series
    lagged_prices <- stats::lag(close_prices, k = lag)

    # Calculate adjusted prices to reduce lag
    adjusted_prices <- close_prices * 2 - lagged_prices

    # Apply EMA to adjusted prices
    zlema_values <- EMA(adjusted_prices, n = n)
    coredata(zlema_values)
  })

  # Convert results to data.frame and then tibble
  zlema_df <- data.frame(zlema_list)
  colnames(zlema_df) <- paste0("zlema_", periods)
  zlema_tibble <- tibble::as_tibble(zlema_df)

  # Return results based on append parameter
  if (append) {
    mktdata_tibble <- tibble::as_tibble(mktdata)
    result_table <- bind_cols(mktdata_tibble, zlema_tibble)
  } else {
    result_table <- zlema_tibble
  }

  return(result_table)
}
