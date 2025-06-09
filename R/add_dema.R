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

#' Calculate and add Double Exponential Moving Averages (DEMA) to stock data
#' @description This function accepts stock data downloaded via quantmod
#' and a variable-length vector of periods (up to 4). It can either add
#' the DEMA columns to the original data or return the DEMA columns separately.
#' @param mktdata xts object containing OHLCV data downloaded via quantmod
#' @param periods numeric vector with 2-4 positive integers representing
#' DEMA periods
#' @param append logical. If TRUE, add DEMA columns to mktdata;
#' otherwise, return only DEMA columns. Default is TRUE.
#' @return
#' If \code{append} is \code{TRUE}, a tibble object with added DEMA columns.
#' If \code{FALSE}, a tibble object with only DEMA columns.
#' @note Input data must contain closing prices (Close), periods must be
#' positive integers
#' @details
#' The function first validates the input data format and the required columns.
#' After that, it calculates the Double Exponential Moving Averages for each specified period.
#' The resulting DEMA values are then combined into a tibble.
#' Finally, depending on the \code{append} parameter, the DEMA tibble is either
#' appended to the original data or returned as a standalone tibble.
#' @keywords DEMA, stock data, technical analysis, quantmod
#' @importFrom tibble tibble
#' @importFrom quantmod Cl
#' @importFrom dplyr bind_cols rename
#' @export
#' @examples
#' # Load internal Tesla stock data
#' data(TSLA)
#' # Calculate and add DEMA columns to TSLA data
#' tsla_with_dema <- add_dema(TSLA, periods = c(10, 20), append = TRUE)
#' head(tsla_with_dema)
#' # Return only DEMA columns for TSLA
#' tsla_only_dema <- add_dema(TSLA, periods = c(10, 20), append = FALSE)
#' head(tsla_only_dema)
add_dema <- function(mktdata, periods = c(50, 200), append = TRUE) {
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

  # Validate the input parameters. Ensure periods are positive integers and the length is within 2 to 4.
  stopifnot(
    "Periods must be 2-4 positive integers" = all(periods > 0) & length(periods) %in% 2:4
  )

  # Extract the closing prices from the market data.
  close_prices <- Cl(mktdata)

  # Calculate DEMA for each period in the periods vector.
  dema_list <- lapply(periods, function(n) {
    dema_values <- DEMA(close_prices, n = n)
    coredata(dema_values)
  })
  # Convert the list of DEMA values to a data.frame.
  dema_df <- data.frame(dema_list)
  # Set the column names of the data.frame based on the periods.
  colnames(dema_df) <- paste0("dema_", periods)
  # Convert the data.frame to a tibble.
  dema_tibble <- tibble::as_tibble(dema_df)

  if (append) {
    # Convert mktdata to a tibble.
    mktdata_tibble <- tibble::as_tibble(mktdata)
    # Combine the mktdata tibble and the DEMA tibble.
    result_table <- bind_cols(mktdata_tibble, dema_tibble)
  } else {
    result_table <- dema_tibble
  }
  return(result_table)
}
