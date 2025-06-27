#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2007 - 2013  Deng Yishuo
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
#' @title Calculate Vertical Horizontal Filter (Optimized)
#' @description
#' The Vertical Horizontal Filter (VHF) attempts to identify starting and ending
#' trends. This optimized version includes safeguards against NA and INF values.
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Number of periods to use. Default is 28.
#' @param na.rm Logical. Should NA values be removed during calculations? Default is TRUE.
#' @param inf.replace Numeric value to replace INF/-INF values. Default is NA.
#' @param zero.replace Numeric value to replace division by zero results. Default is NA.
#' @param allow_middle_na Logical. If TRUE, allows NA values in the middle of the series.
#' @param append A logical value. If \code{TRUE}, the calculated VHF values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' VHF values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the VHF values with NA/INF values handled.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated VHF values appended, maintaining the integrity of the time - series
#' alignment.
#' @note If Close prices are given, the function calculates the max/min using
#' only those prices. If HLC prices are given, the function uses high/low prices.
#' @author Deng Yishuo
#' @seealso See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{ADX}},
#' \code{\link{TDI}}, \code{\link{GMMA}} for other trend indicators.
#' @references \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=119}
#' @keywords ts
#' @examples
#' \dontrun{
#' # Example 1: Using closing prices
#' data("AAPL")
#' close_price <- Cl(AAPL)
#' vhf_close <- add_VHF(AAPL, n = 14)
#' chartSeries(AAPL, TA = "addTA(vhf_close,col='blue')")
#'
#' # Example 2: Using HLC data
#' hlc_data <- HLC(AAPL)
#' vhf_hlc <- add_VHF(AAPL, n = 28, inf.replace = 0)
#'
#' # Example 3: Handling NA values
#' test_data <- c(1:10, NA, 12:20)
#' vhf_na <- add_VHF(test_data, n = 5, na.rm = TRUE)
#'
#' # Example 4: Zero - division protection
#' zero_test <- c(1, 1, 1, 1, 1)
#' vhf_zero <- add_VHF(zero_test, zero.replace = 0)
#' }
#' @export
add_VHF <- function(OHLCV, n = 28, na.rm = TRUE, inf.replace = NA, zero.replace = NA, allow_middle_na = FALSE, append = FALSE) {
  # Validate inputs
  if (is.null(OHLCV)) stop("OHLCV cannot be NULL")
  if (n <= 0 || !is.numeric(n)) stop("n must be a positive number")
  if (!is.logical(na.rm) || length(na.rm) != 1) stop("na.rm must be a logical value")
  if (!is.numeric(inf.replace) && !is.na(inf.replace)) stop("inf.replace must be a numeric value or NA")
  if (!is.numeric(zero.replace) && !is.na(zero.replace)) stop("zero.replace must be a numeric value or NA")
  if (!is.logical(allow_middle_na) || length(allow_middle_na) != 1) {
    stop("allow_middle_na must be a logical value")
  }

  # Assume we use Close or High - Low - Close prices for calculation, can be adjusted
  if (NCOL(OHLCV) == 1) {
    price <- OHLCV
  } else if (NCOL(OHLCV) >= 3) {
    price <- OHLCV[, c("High", "Low", "Close")]
  } else {
    stop("Price series must have at least 1 (Close) or 3 (High - Low - Close) columns")
  }

  # Convert price to xts or matrix
  price <- try.xts(price, error = as.matrix)
  price_len <- nrow(price)

  # Check if n is within valid range
  if (n >= price_len) stop("n must be smaller than data length")

  # Handle NA values
  if (anyNA(price)) {
    # Find first non - NA position
    first_non_na <- min(which(!is.na(price[, 1])))

    # Check for non - leading NA values
    if (!allow_middle_na && anyNA(price[first_non_na:price_len, ])) {
      stop("VHF requires all NA values to be leading when allow_middle_na is FALSE")
    }

    # Trim leading NA values
    if (first_non_na > 1) {
      price <- price[first_non_na:price_len, ]
      price_len <- nrow(price)

      # Check if remaining data is sufficient
      if (price_len < n) {
        stop("insufficient non - NA data after removing leading NA")
      }
    }
  }

  # Extract high, low, close prices
  if (NCOL(price) == 1) {
    high <- price
    low <- price
    close <- price
  } else if (NCOL(price) == 3) {
    high <- price[, 1]
    low <- price[, 2]
    close <- price[, 3]
  }

  # Calculate high max and low min over n periods
  hmax <- runMax(high, n)
  lmin <- runMin(low, n)

  # Calculate price range (high max - low min)
  price_range <- hmax - lmin

  # Calculate price differences
  deltas <- diff(close)

  # Calculate sum of absolute price changes over n periods
  price_change <- runSum(abs(deltas), n)

  # Calculate VHF with zero division protection
  vhf <- price_range / pmax(price_change, 1e-10) * 100

  # Apply NA/INF replacements
  if (na.rm) {
    vhf[is.na(vhf)] <- zero.replace
  }

  if (!is.na(inf.replace)) {
    vhf[is.infinite(vhf)] <- inf.replace
  }

  # Handle zero division replacements
  if (!is.na(zero.replace)) {
    zero_indices <- which(price_change < 1e-10)
    if (length(zero_indices) > 0) {
      vhf[zero_indices] <- zero.replace
    }
  }

  # Convert to vector and reclass to match input
  result <- as.vector(vhf)
  result <- reclass(result, price)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, result)
    return(combined_result)
  } else {
    return(result)
  }
}
