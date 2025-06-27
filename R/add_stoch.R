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
#' @title Calculate Stochastic Oscillator
#' @description Calculates the fast %K, fast %D, and slow %D values of
#' the stochastic oscillator.
#' @param OHLCV Object coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param nFastK Number of periods for fast %K calculation (default: 14).
#' @param nFastD Number of periods for fast %D calculation (default: 3).
#' @param nSlowD Number of periods for slow %D calculation (default: 3).
#' @param maType Moving average type: function, string, or list of configurations
#' (default: "SMA").
#' @param bounded Logical, use current period in range calculation (default: TRUE).
#' @param smooth Internal smoothing periods for FastK (default: 1).
#' @param append A logical value. If \code{TRUE}, the calculated Stochastic Oscillator
#' values (fast %K, fast %D, slow %D) will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Stochastic Oscillator values will be returned. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to moving average functions.
#' @return If \code{append = FALSE}, an xts or matrix object with columns: fastK, fastD, slowD.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Stochastic Oscillator values appended, maintaining the integrity of the time - series
#' alignment.
#' @author DengYishuo
#' @keywords ts momentum indicator
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' stoch_result1 <- add_stoch(TSLA)
#'
#' # Modifying nFastK and without appending
#' stoch_result2 <- add_stoch(TSLA, nFastK = 10)
#'
#' # Using default parameters and appending
#' stoch_result3 <- add_stoch(TSLA, append = TRUE)
#'
#' # Modifying nFastK and appending
#' stoch_result4 <- add_stoch(TSLA, nFastK = 10, append = TRUE)
#' }
#' @importFrom xts try.xts reclass
#' @export
add_stoch <- function(OHLCV, nFastK = 14, nFastD = 3, nSlowD = 3,
                      maType, bounded = TRUE, smooth = 1, append = FALSE, ...) {
  # Assume we use High - Low - Close prices for calculation, can be adjusted
  hlc <- OHLCV[, c("High", "Low", "Close")]
  hlc <- try.xts(hlc, error = as.matrix)

  # Extract price components
  if (NCOL(hlc) == 3) {
    high <- hlc[, 1]
    low <- hlc[, 2]
    close <- hlc[, 3]
  } else if (NCOL(hlc) == 1) {
    high <- hlc
    low <- hlc
    close <- hlc
  } else {
    stop("Price series must be High - Low - Close or univariate")
  }

  # Calculate high and low ranges
  if (bounded) {
    hmax <- runMax(high, nFastK)
    lmin <- runMin(low, nFastK)
  } else {
    hmax <- runMax(c(high[1], high[-NROW(hlc)]), nFastK)
    lmin <- runMin(c(low[1], low[-NROW(hlc)]), nFastK)
  }

  # Handle edge cases for division
  num <- close - lmin
  den <- hmax - lmin
  den[den == 0] <- 1 # Prevent division by zero

  # Set default moving average type
  if (missing(maType)) {
    maType <- "SMA"
  }

  # Process moving average configurations
  if (is.list(maType)) {
    # Validate list structure for three moving averages
    if (!all(sapply(maType, is.list)) || length(maType) != 3) {
      stop("maType list must contain three moving average configurations")
    }

    # Populate missing 'n' parameters
    if (!is.null(formals(maType[[1]][[1]])$n) && is.null(maType[[1]]$n)) {
      maType[[1]]$n <- nFastD
    }
    if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
      maType[[2]]$n <- nSlowD
    }
    if (!is.null(formals(maType[[3]][[1]])$n) && is.null(maType[[3]]$n)) {
      maType[[3]]$n <- smooth
    }

    # Apply moving averages
    numMA <- do.call(maType[[3]][[1]], c(list(num), maType[[3]][-1]))
    denMA <- do.call(maType[[3]][[1]], c(list(den), maType[[3]][-1]))
    fastK <- numMA / denMA
    fastK[!is.finite(fastK) & !is.na(fastK)] <- 0.5
    fastD <- do.call(maType[[1]][[1]], c(list(fastK), maType[[1]][-1]))
    slowD <- do.call(maType[[2]][[1]], c(list(fastD), maType[[2]][-1]))
  } else {
    # Single moving average type
    numMA <- do.call(maType, c(list(num), list(n = smooth, ...)))
    denMA <- do.call(maType, c(list(den), list(n = smooth, ...)))
    fastK <- numMA / denMA
    fastK[!is.finite(fastK) & !is.na(fastK)] <- 0.5
    fastD <- do.call(maType, c(list(fastK), list(n = nFastD, ...)))
    slowD <- do.call(maType, c(list(fastD), list(n = nSlowD, ...)))
  }

  # Construct result
  result <- cbind(fastK, fastD, slowD)
  colnames(result) <- c("fastK", "fastD", "slowD")
  result <- reclass(result, hlc)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, result)
    return(combined_result)
  } else {
    return(result)
  }
}
