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

#' Calculate and add Arnaud Legoux Moving Averages (ALMA) to stock data
#' @description This function calculates ALMA, a highly efficient moving average
#' that uses a Gaussian distribution to weight data points, reducing lag and
#' providing excellent noise reduction.
#' @param mktdata xts object containing OHLCV data downloaded via quantmod
#' @param periods numeric vector with 2-4 positive integers representing ALMA periods
#' @param offset numeric between 0 and 1 determining the position of the weight
#' distribution. Default is 0.85, which provides good balance between lag and noise.
#' @param sigma numeric specifying the width of the Gaussian filter. Default is 6.0.
#' @param append logical. If TRUE, add ALMA columns to mktdata; otherwise, return only ALMA columns. Default is TRUE.
#' @return
#' If \code{append} is \code{TRUE}, a tibble object with added ALMA columns.
#' If \code{FALSE}, a tibble object with only ALMA columns.
#' @note Input data must contain closing prices (Close)
#' @details
#' ALMA is calculated using a Gaussian distribution to weight data points,
#' with the peak of the distribution determined by the offset parameter.
#' This allows the moving average to react quickly to price changes while
#' maintaining smoothness. The calculation steps are:
#'
#' 1. **Determine the peak position (m)**:
#'    \[
#'    m = \text{floor}(\text{offset} \cdot (n-1))
#'    \]
#'    where \code{n} is the period and \code{offset} controls the weight
#'    distribution center (0.85 by default, favoring recent prices).
#'
#' 2. **Compute Gaussian weights**:
#'    \[
#'    w_i = \exp\left(-\frac{(i - m)^2}{2\sigma^2}\right)
#'    \]
#'    for each data point \code{i} in the window, where \code{\sigma}
#'    (default 6.0) controls the width of the distribution.
#'
#' 3. **Normalize weights**:
#'    \[
#'    \text{Normalized } w_i = \frac{w_i}{\sum_{i=0}^{n-1} w_i}
#'    \]
#'
#' 4. **Calculate weighted average**:
#'    \[
#'    \text{ALMA}_t = \sum_{i=0}^{n-1} (\text{Normalized } w_i \cdot \text{Price}_{t-i})
#'    \]
#'
#' Higher \code{offset} values prioritize recent prices, reducing lag but
#' increasing sensitivity to noise. Lower \code{sigma} values create a
#' tighter weight distribution, focusing on fewer data points.
#' @keywords ALMA, Arnaud Legoux, technical analysis, quantmod
#' @importFrom tibble tibble
#' @importFrom quantmod Cl
#' @importFrom zoo rollapply
#' @importFrom dplyr bind_cols
#' @export
#' @examples
#' library(quantmod)
#' library(tibble)
#' library(zoo)
#' library(dplyr)
#' # Load internal Tesla stock data
#' data(TSLA)
#' # Calculate and add ALMA columns to TSLA data with default parameters
#' tsla_with_alma <- add_alma(TSLA, periods = c(10, 20))
#' head(tsla_with_alma)
#' # Calculate ALMA with custom offset and sigma
#' tsla_alma_custom <- add_alma(TSLA, periods = 20, offset = 0.5, sigma = 4.0, append = FALSE)
#' head(tsla_alma_custom)
add_alma <- function(mktdata, periods = c(50, 200), offset = 0.85, sigma = 6.0, append = TRUE) {
  # Validate input data format
  if (!inherits(mktdata, "xts")) {
    stop("mktdata must be an xts object")
  }

  # Check for required columns (Close)
  required_cols <- c("Close")
  if (!all(required_cols %in% colnames(mktdata))) {
    stop("mktdata must contain 'Close' column")
  }

  # Validate parameters
  stopifnot(
    "Periods must be 2-4 positive integers" = all(periods > 0) & length(periods) %in% 2:4,
    "Offset must be between 0 and 1" = offset >= 0 & offset <= 1,
    "Sigma must be positive" = sigma > 0
  )

  # Extract closing prices
  close_prices <- Cl(mktdata)

  # Function to calculate ALMA for a single period
  calculate_alma <- function(prices, n, offset, sigma) {
    # Calculate m parameter (location of the maximum weight)
    m <- floor(offset * (n - 1))

    # Calculate sigma squared
    s2 <- sigma^2

    # Function to compute ALMA weights
    alma_weights <- function(x) {
      k <- seq_along(x) - 1
      weights <- exp(-((k - m)^2) / (2 * s2))
      weights / sum(weights)
    }

    # Apply rolling ALMA calculation
    rollapplyr(prices, width = n, FUN = function(w) {
      weights <- alma_weights(w)
      sum(w * weights)
    }, fill = NA)
  }

  # Calculate ALMA for each period
  alma_list <- lapply(periods, function(n) {
    alma_values <- calculate_alma(close_prices, n, offset, sigma)
    coredata(alma_values)
  })

  # Convert results to data.frame and then tibble
  alma_df <- data.frame(alma_list)
  colnames(alma_df) <- paste0("alma_", periods)
  alma_tibble <- tibble::as_tibble(alma_df)

  # Return results based on append parameter
  if (append) {
    mktdata_tibble <- tibble::as_tibble(mktdata)
    result_table <- bind_cols(mktdata_tibble, alma_tibble)
  } else {
    result_table <- alma_tibble
  }

  return(result_table)
}
