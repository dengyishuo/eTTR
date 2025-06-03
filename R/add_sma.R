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
#' Calculate and add Simple Moving Averages (SMA) to stock data
#' @description This function accepts stock data downloaded via quantmod
#' and a variable - length vector of periods (up to 4). It can either add
#' the SMA columns to the original data or return the SMA columns separately.
#' @param mktdata xts object containing OHLCV data downloaded via quantmod
#' @param periods numeric vector with 2 - 4 positive integers representing
#' SMA periods
#' @param append logical. If TRUE, add SMA columns to mktdata;
#' otherwise, return only SMA columns. Default is TRUE.
#' @return
#' If \code{append} is \code{TRUE}, a tibble object with added SMA columns.
#' If \code{FALSE}, a tibble object with only SMA columns.
#' @note Input data must contain closing prices (Close), periods must be
#' positive integers
#' @details
#' The function first validates the input data format and the required columns.
#' It then checks if the TTR package is available and installs/loads it if not.
#' After that, it calculates the Simple Moving Averages for each specified period.
#' The resulting SMA values are then combined into a tibble.
#' Finally, depending on the \code{append} parameter, the SMA tibble is either
#' appended to the original data or returned as a standalone tibble.
#' @keywords SMA, stock data, technical analysis, quantmod, TTR
#' @importFrom tibble tibble
#' @importFrom quantmod Cl
#' @importFrom dplyr bind_cols rename
#' @export
#' @examples
#' library(quantmod)
#' library(tibble)
#' library(TTR)
#' library(dplyr)
#' # Download stock data (example using AAPL)
#' getSymbols("AAPL")
#' # Calculate and add SMA columns to AAPL data
#' aapl_with_sma <- add_sma(AAPL, periods = c(10, 20), append = TRUE)
#' head(aapl_with_sma)
#' # Return only SMA columns for AAPL
#' aapl_only_sma <- add_sma(AAPL, periods = c(10, 20), append = FALSE)
#' head(aapl_only_sma)
add_sma <- function(mktdata, periods = c(50, 200), append = TRUE) {
  # Check the data format. If mktdata is not of xts type, stop the function.
  if (!inherits(mktdata, "xts")) {
    stop("mktdata must be an xts object")
  }
  # Define the required columns for the data.
  required_cols <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  # If not all required columns are in mktdata, stop the function.
  if (!all(required_cols %in% colnames(mktdata))) {
    stop(
      "mktdata must contain all of the following columns: ",
      paste(required_cols, collapse = ", ")
    )
  }

  # Check if the TTR package is available. If not, install and load it.
  if (!requireNamespace("TTR", quietly = TRUE)) {
    install.packages("TTR")
    library(TTR)
  }

  # Validate the input parameters. Ensure periods are positive integers and the length is within 2 to 4.
  stopifnot(
    "Periods must be 2 - 4 positive integers" = all(periods > 0) & length(periods) %in% 2:4
  )

  # Extract the closing prices from the market data.
  close_prices <- Cl(mktdata)

  # Calculate SMA for each period in the periods vector.
  sma_list <- lapply(periods, function(n) {
    sma_values <- SMA(close_prices, n = n)
    coredata(sma_values)
  })
  # Convert the list of SMA values to a data.frame.
  sma_df <- data.frame(sma_list)
  # Set the column names of the data.frame based on the periods.
  colnames(sma_df) <- paste0("sma_", periods)
  # Convert the data.frame to a tibble.
  sma_tibble <- tibble::as_tibble(sma_df)

  if (append) {
    # Convert mktdata to a tibble.
    mktdata_tibble <- tibble::as_tibble(mktdata)
    # Combine the mktdata tibble and the SMA tibble.
    result_table <- bind_cols(mktdata_tibble, sma_tibble)
  } else {
    result_table <- sma_tibble
  }
  return(result_table)
}
