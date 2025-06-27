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
#' @title Calculate Arnaud Legoux Moving Average (ALMA)
#' @description
#' Calculate a Gaussian - weighted moving average with reduced lag. The Arnaud Legoux Moving Average (ALMA)
#' uses a Gaussian - shaped weight distribution to calculate the moving average. This results in a more
#' responsive moving average compared to traditional moving averages, as it can be adjusted to emphasize
#' recent prices more effectively.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' The function will extract the closing price from this object for ALMA calculation.
#' @param n Number of periods to average over. This parameter determines the window size of the moving average.
#' A larger \code{n} will result in a smoother ALMA, but it may be less responsive to recent price changes.
#' @param offset Percentile for weight distribution center (0 - 1). A higher \code{offset} value emphasizes
#' more recent prices in the moving average calculation. For example, an \code{offset} close to 1 will
#' give more weight to the most recent data points.
#' @param sigma Standard deviation of the Gaussian distribution. A lower \code{sigma} reduces the smoothing
#' effect, making the ALMA more sensitive to short - term price fluctuations.
#' @param append A logical value. If \code{TRUE}, the calculated ALMA values will be appended to the \code{OHLCV} input data,
#' ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
#' ALMA values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
#' containing the ALMA values.
#' If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated ALMA values appended,
#' maintaining the integrity of the time - series alignment.
#' @note
#' Higher \code{offset} emphasizes recent prices; lower \code{sigma} reduces smoothing.
#' It's important to note that the choice of \code{offset} and \code{sigma} can significantly impact the
#' behavior of the ALMA. Traders should carefully consider these parameters based on the characteristics
#' of the price series and their trading strategies.
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' alma_result1 <- add_ALMA(TSLA)
#'
#' # Using default parameters and appending
#' alma_result2 <- add_ALMA(TSLA, append = TRUE)
#'
#' # Changing n and without appending
#' alma_result3 <- add_ALMA(TSLA, n = 12)
#'
#' # Changing n and appending
#' alma_result4 <- add_ALMA(TSLA, n = 12, append = TRUE)
#'
#' # Changing offset and without appending
#' alma_result5 <- add_ALMA(TSLA, offset = 0.9)
#'
#' # Changing sigma and without appending
#' alma_result6 <- add_ALMA(TSLA, sigma = 4)
#' }
add_ALMA <- function(OHLCV, n = 9, offset = 0.85, sigma = 6, append = FALSE) {
  # Check if OHLCV contains 'Close' column
  if (!"Close" %in% colnames(OHLCV)) {
    stop("OHLCV must contain 'Close' column")
  }

  # Extract the closing price
  x <- OHLCV[, "Close"]
  x <- try.xts(x, error = as.matrix)

  # Validate offset
  if (offset < 0 || offset > 1) {
    stop("Please ensure 0 <= offset <= 1")
  }

  # Validate sigma
  if (sigma <= 0) {
    stop("sigma must be > 0")
  }

  # Calculate parameters for Gaussian weights
  m <- floor(offset * (n - 1))
  s <- n / sigma
  wts <- exp(-((seq(0, n - 1) - m)^2) / (2 * s * s))
  sumWeights <- sum(wts)
  if (sumWeights != 0) {
    wts <- wts / sumWeights
  }

  alma <- x * NA_real_
  for (i in seq_len(NCOL(x))) {
    alma[, i] <- WMA(x[, i], n, wts)
  }

  if (!is.null(dim(alma))) {
    colnames(alma) <- "ALMA"
  }

  alma <- reclass(alma, x)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, alma)
    return(combined_result)
  } else {
    return(alma)
  }
}
