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

#' Calculate and add Weighted Moving Averages (WMA) to stock data
#' @description This function accepts stock data downloaded via quantmod
#' and a variable-length vector of periods (up to 4). It can either add
#' the WMA columns to the original data or return the WMA columns separately.
#' @param mktdata xts object containing OHLCV data downloaded via quantmod
#' @param periods numeric vector with 2-4 positive integers representing
#' WMA periods
#' @param append logical. If TRUE, add WMA columns to mktdata;
#' otherwise, return only WMA columns. Default is TRUE.
#' @return
#' If \code{append} is \code{TRUE}, a tibble object with added WMA columns.
#' If \code{FALSE}, a tibble object with only WMA columns.
#' @note Input data must contain closing prices (Close), periods must be
#' positive integers
#' @details
#' The function first validates the input data format and the required columns.
#' It then checks if the TTR package is available and installs/loads it if not.
#' After that, it calculates the Weighted Moving Averages for each specified period.
#' The resulting WMA values are then combined into a tibble.
#' Finally, depending on the \code{append} parameter, the WMA tibble is either
#' appended to the original data or returned as a standalone tibble.
#' @keywords WMA, stock data, technical analysis, quantmod, TTR
#' @importFrom tibble tibble
#' @importFrom quantmod Cl
#' @importFrom dplyr bind_cols rename
#' @export
#' @examples
#' library(quantmod)
#' library(tibble)
#' library(TTR)
#' library(dplyr)
#' # Load internal Tesla stock data
#' data(TSLA)
#' # Calculate and add WMA columns to TSLA data
#' tsla_with_wma <- add_wma(TSLA, periods = c(10, 20), append = TRUE)
#' head(tsla_with_wma)
#' # Return only WMA columns for TSLA
#' tsla_only_wma <- add_wma(TSLA, periods = c(10, 20), append = FALSE)
#' head(tsla_only_wma)
add_wma <- function(mktdata, periods = c(50, 200), append = TRUE) {
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
    "Periods must be 2-4 positive integers" = all(periods > 0) & length(periods) %in% 2:4
  )

  # Extract the closing prices from the market data.
  close_prices <- Cl(mktdata)

  # Calculate WMA for each period in the periods vector.
  wma_list <- lapply(periods, function(n) {
    wma_values <- WMA(close_prices, n = n)
    coredata(wma_values)
  })
  # Convert the list of WMA values to a data.frame.
  wma_df <- data.frame(wma_list)
  # Set the column names of the data.frame based on the periods.
  colnames(wma_df) <- paste0("wma_", periods)
  # Convert the data.frame to a tibble.
  wma_tibble <- tibble::as_tibble(wma_df)

  if (append) {
    # Convert mktdata to a tibble.
    mktdata_tibble <- tibble::as_tibble(mktdata)
    # Combine the mktdata tibble and the WMA tibble.
    result_table <- bind_cols(mktdata_tibble, wma_tibble)
  } else {
    result_table <- wma_tibble
  }
  return(result_table)
}
