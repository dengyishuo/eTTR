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
#' Calculate Stochastic Oscillator (KDJ) Indicator
#'
#' This function computes the KDJ indicator based on an OHLC (Open, High, Low, Close) xts object,
#' automatically extracting required columns ('high', 'low', 'close') from the input (case-insensitive).
#' The KDJ indicator helps identify overbought or oversold conditions, returning K, D, and J lines.
#'
#' @param ohlc xts object with OHLC data, must include 'high', 'low', and 'close' columns (case-insensitive).
#' @param n RSV calculation period, default is 9.
#' @param m1 Smoothing factor for K line, default is 3.
#' @param m2 Smoothing factor for D line, default is 3.
#' @param fill_na_method Method to fill NA values: "none", "initial", "interpolate".
#' @return An xts object with three columns: 'K', 'D', and 'J'.
#' @importFrom xts xts is.xts
#' @importFrom zoo rollapply na.approx index
#' @examples
#' data(TSLA)
#' tsla_kdj <- KDJ(TSLA, n = 9, m1 = 3, m2 = 3)
#' @export
KDJ <- function(ohlc, n = 9, m1 = 3, m2 = 3, fill_na_method = "none") {
  if (!xts::is.xts(ohlc)) stop("Input 'ohlc' must be an xts object.")

  input_cols <- tolower(colnames(ohlc))
  required_cols <- c("high", "low", "close")

  missing_cols <- required_cols[!required_cols %in% input_cols]
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  high_idx <- which(input_cols == "high")
  low_idx <- which(input_cols == "low")
  close_idx <- which(input_cols == "close")

  high <- ohlc[, high_idx]
  low <- ohlc[, low_idx]
  close <- ohlc[, close_idx]

  time_index <- zoo::index(ohlc)
  len <- length(close)

  highest_high <- zoo::rollapply(high, n, max, align = "right", fill = NA)
  lowest_low <- zoo::rollapply(low, n, min, align = "right", fill = NA)

  rsv <- xts::xts(rep(NA, len), order.by = time_index)
  valid_idx <- which(!is.na(highest_high) & !is.na(lowest_low) & (highest_high != lowest_low))
  zero_div_idx <- which(!is.na(highest_high) & !is.na(lowest_low) & (highest_high == lowest_low))

  rsv[valid_idx] <- 100 * (close[valid_idx] - lowest_low[valid_idx]) /
    (highest_high[valid_idx] - lowest_low[valid_idx])
  rsv[zero_div_idx] <- 50


  k <- xts::xts(rep(NA, len), order.by = time_index)
  d <- xts::xts(rep(NA, len), order.by = time_index)
  j <- xts::xts(rep(NA, len), order.by = time_index)


  first_valid <- which(!is.na(rsv))[1]
  if (is.na(first_valid)) {
    warning("No valid RSV values. Returning all NA.")
    return(xts::xts(matrix(NA, nrow = len, ncol = 3),
      order.by = time_index,
      dimnames = list(NULL, c("K", "D", "J"))
    ))
  }

  k[first_valid] <- 50
  d[first_valid] <- 50

  for (i in (first_valid + 1):len) {
    prev_k <- ifelse(is.na(k[i - 1]), 50, k[i - 1])
    prev_d <- ifelse(is.na(d[i - 1]), 50, d[i - 1])

    k[i] <- (1 / m1) * rsv[i] + (1 - 1 / m1) * prev_k
    d[i] <- (1 / m2) * k[i] + (1 - 1 / m2) * prev_d
  }

  j <- 3 * k - 2 * d

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

  result <- cbind(K = k, D = d, J = j)
  return(result)
}
