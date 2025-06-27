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
#' @title Calculate Stochastic Oscillator (KDJ) Indicator
#' @description
#' This function computes the KDJ indicator based on an OHLC (Open, High, Low, Close) xts object.
#' It automatically extracts the required columns ('high', 'low', 'close') from the input (case - insensitive).
#' The KDJ indicator is used to identify overbought or oversold conditions in the market.
#' It returns the K, D, and J lines which can help traders make decisions about entry and exit points.
#'
#' @param OHLCV xts object with OHLC data, must include 'high', 'low', and 'close' columns (case - insensitive).
#' @param n RSV calculation period, default is 9. This parameter determines the look - back period
#' for calculating the Raw Stochastic Value (RSV). A shorter period makes the indicator more sensitive
#' to recent price changes, while a longer period provides a smoother view of the market trend.
#' @param m1 Smoothing factor for K line, default is 3. A smaller value of \code{m1} makes the K line
#' more responsive to recent price changes, while a larger value smooths out the line.
#' @param m2 Smoothing factor for D line, default is 3. Similar to \code{m1}, it affects the smoothness
#' and responsiveness of the D line.
#' @param fill_na_method Method to fill NA values: "none", "initial", "interpolate".
#' "none" leaves NA values as they are. "initial" fills the initial NA values with a default value (50).
#' "interpolate" fills NA values using linear interpolation.
#' @param append A logical value. If \code{TRUE}, the calculated KDJ values (K, D, J) will be appended to the \code{OHLCV} input data,
#' ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated KDJ values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append} is \code{FALSE}, an xts object with three columns: 'K', 'D', and 'J'.
#' If \code{append} is \code{TRUE}, an xts object with the original \code{OHLCV} data and the calculated KDJ values appended,
#' maintaining the integrity of the time - series alignment.
#' @importFrom xts xts is.xts
#' @importFrom zoo rollapply na.approx index
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' kdj_result1 <- add_KDJ(TSLA)
#'
#' # Using default parameters and appending
#' kdj_result2 <- add_KDJ(TSLA, append = TRUE)
#'
#' # Changing n and without appending
#' kdj_result3 <- add_KDJ(TSLA, n = 12)
#'
#' # Changing n and appending
#' kdj_result4 <- add_KDJ(TSLA, n = 12, append = TRUE)
#' }
add_KDJ <- function(OHLCV, n = 9, m1 = 3, m2 = 3, fill_na_method = "none", append = FALSE) {
  # Check if OHLCV is an xts object
  if (!xts::is.xts(OHLCV)) stop("Input 'OHLCV' must be an xts object.")

  # Convert column names to lowercase for case - insensitive matching
  input_cols <- tolower(colnames(OHLCV))
  required_cols <- c("high", "low", "close")

  # Check for missing required columns
  missing_cols <- required_cols[!required_cols %in% input_cols]
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Find the indices of required columns
  high_idx <- which(input_cols == "high")
  low_idx <- which(input_cols == "low")
  close_idx <- which(input_cols == "close")

  # Extract high, low, and close prices
  high <- OHLCV[, high_idx]
  low <- OHLCV[, low_idx]
  close <- OHLCV[, close_idx]

  # Get the time index of the OHLCV data
  time_index <- zoo::index(OHLCV)
  len <- length(close)

  # Calculate the highest high and lowest low over the n - period
  highest_high <- zoo::rollapply(high, n, max, align = "right", fill = NA)
  lowest_low <- zoo::rollapply(low, n, min, align = "right", fill = NA)

  # Initialize RSV with NA values
  rsv <- xts::xts(rep(NA, len), order.by = time_index)
  # Calculate RSV for valid indices
  valid_idx <- which(!is.na(highest_high) & !is.na(lowest_low) & (highest_high != lowest_low))
  zero_div_idx <- which(!is.na(highest_high) & !is.na(lowest_low) & (highest_high == lowest_low))

  rsv[valid_idx] <- 100 * (close[valid_idx] - lowest_low[valid_idx]) /
    (highest_high[valid_idx] - lowest_low[valid_idx])
  rsv[zero_div_idx] <- 50

  # Initialize K, D, and J with NA values
  k <- xts::xts(rep(NA, len), order.by = time_index)
  d <- xts::xts(rep(NA, len), order.by = time_index)
  j <- xts::xts(rep(NA, len), order.by = time_index)

  # Find the first valid RSV value
  first_valid <- which(!is.na(rsv))[1]
  if (is.na(first_valid)) {
    warning("No valid RSV values. Returning all NA.")
    return(xts::xts(matrix(NA, nrow = len, ncol = 3),
      order.by = time_index,
      dimnames = list(NULL, c("K", "D", "J"))
    ))
  }

  # Initialize the first valid K and D values
  k[first_valid] <- 50
  d[first_valid] <- 50

  # Calculate K and D values iteratively
  for (i in (first_valid + 1):len) {
    prev_k <- ifelse(is.na(k[i - 1]), 50, k[i - 1])
    prev_d <- ifelse(is.na(d[i - 1]), 50, d[i - 1])

    k[i] <- (1 / m1) * rsv[i] + (1 - 1 / m1) * prev_k
    d[i] <- (1 / m2) * k[i] + (1 - 1 / m2) * prev_d
  }

  # Calculate J value
  j <- 3 * k - 2 * d

  # Fill NA values based on fill_na_method
  if (fill_na_method == "initial" && first_valid > 1) {
    initial_val <- 50
    k[1:(first_valid - 1)] <- initial_val
    d[1:(first_valid - 1)] <- initial_val
    j[1:(first_valid - 1)] <- 3 * initial_val - 2 * initial_val
  } else if (fill_na_method == "interpolate") {
    k <- zoo::na.approx(k, na.rm = FALSE)
    d <- zoo::na.approx(d, na.rm = FALSE)
    j <- 3 * k - 2 * d
  }

  # Combine K, D, and J into a single xts object
  result <- cbind(K = k, D = d, J = j)

  if (append) {
    # Append the KDJ values to the original OHLCV data
    combined_result <- cbind(OHLCV, result)
    return(combined_result)
  } else {
    return(result)
  }
}
