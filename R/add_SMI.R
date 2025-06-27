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
#' @title Calculate Stochastic Momentum Index
#' @description Calculates the SMI value and its signal line.
#' @param OHLCV Object coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Number of periods for range calculation (default: 13).
#' @param nFast Fast moving average periods (default: 2).
#' @param nSlow Slow moving average periods (default: 25).
#' @param nSig Signal line periods (default: 9).
#' @param maType Moving average type: function, string, or list of configurations
#' (default: "EMA").
#' @param bounded Logical, use current period in range calculation (default: TRUE).
#' @param append A logical value. If \code{TRUE}, the calculated Stochastic Momentum Index
#' values and its signal line will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Stochastic Momentum Index values and its signal line will be returned. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to moving average functions.
#' @return If \code{append = FALSE}, an xts or matrix object with columns: SMI, signal.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Stochastic Momentum Index values and its signal line appended, maintaining the integrity of the time - series
#' alignment.
#' @author DengYishuo
#' @keywords ts momentum indicator
#' @importFrom xts try.xts reclass
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' smi_result1 <- add_SMI(TSLA)
#'
#' # Modifying n and without appending
#' smi_result2 <- add_SMI(TSLA, n = 15)
#'
#' # Using default parameters and appending
#' smi_result3 <- add_SMI(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' smi_result4 <- add_SMI(TSLA, n = 15, append = TRUE)
#' }
#' @export
add_SMI <- function(OHLCV, n = 13, nFast = 2, nSlow = 25, nSig = 9,
                    maType, bounded = TRUE, append = FALSE, ...) {
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
    hmax <- runMax(high, n)
    lmin <- runMin(low, n)
  } else {
    hmax <- runMax(c(high[1], high[-NROW(hlc)]), n)
    lmin <- runMin(c(low[1], low[-NROW(hlc)]), n)
  }

  # Handle NA values in initial ranges
  hmax <- ifelse(is.na(hmax), high, hmax)
  lmin <- ifelse(is.na(lmin), low, lmin)

  # Calculate SMI components
  hldiff <- hmax - lmin
  cdiff <- close - (hmax + lmin) / 2

  # Set default moving average type
  if (missing(maType)) {
    maType <- "EMA"
  }

  # Process moving average configurations
  if (is.list(maType)) {
    # Validate list structure for three moving averages
    if (!all(sapply(maType, is.list)) || length(maType) != 3) {
      stop("maType list must contain three moving average configurations")
    }

    # Populate missing 'n' parameters
    if (!is.null(formals(maType[[1]][[1]])$n) && is.null(maType[[1]]$n)) {
      maType[[1]]$n <- nFast
    }
    if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
      maType[[2]]$n <- nSlow
    }
    if (!is.null(formals(maType[[3]][[1]])$n) && is.null(maType[[3]]$n)) {
      maType[[3]]$n <- nSig
    }

    # Apply moving averages
    num1 <- do.call(maType[[1]][[1]], c(list(cdiff), maType[[1]][-1]))
    den1 <- do.call(maType[[1]][[1]], c(list(hldiff), maType[[1]][-1]))
    num2 <- do.call(maType[[2]][[1]], c(list(num1), maType[[2]][-1]))
    den2 <- do.call(maType[[2]][[1]], c(list(den1), maType[[2]][-1]))

    smi <- 100 * (num2 / (den2 / 2))
    signal <- do.call(maType[[3]][[1]], c(list(smi), maType[[3]][-1]))
  } else {
    # Single moving average type
    num1 <- do.call(maType, c(list(cdiff), list(n = nSlow, ...)))
    den1 <- do.call(maType, c(list(hldiff), list(n = nSlow, ...)))
    num2 <- do.call(maType, c(list(num1), list(n = nFast, ...)))
    den2 <- do.call(maType, c(list(den1), list(n = nFast, ...)))

    smi <- 100 * (num2 / (den2 / 2))
    signal <- do.call(maType, c(list(smi), list(n = nSig, ...)))
  }

  # Construct result
  result <- cbind(smi, signal)
  colnames(result) <- c("SMI", "signal")
  result <- reclass(result, hlc)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, result)
    return(combined_result)
  } else {
    return(result)
  }
}
