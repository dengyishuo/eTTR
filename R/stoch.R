#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2025-2030  DengYishuo
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
#' @title Stochastic Oscillator
#' @description Calculates the fast %K, fast %D, and slow %D values of
#' the stochastic oscillator.
#'
#' @param HLC Object coercible to xts or matrix, containing High-Low-Close prices
#' or a univariate series.
#' @param nFastK Number of periods for fast %K calculation (default: 14).
#' @param nFastD Number of periods for fast %D calculation (default: 3).
#' @param nSlowD Number of periods for slow %D calculation (default: 3).
#' @param maType Moving average type: function, string, or list of configurations
#' (default: "SMA").
#' @param bounded Logical, use current period in range calculation (default: TRUE).
#' @param smooth Internal smoothing periods for FastK (default: 1).
#' @param ... Additional arguments passed to moving average functions.
#'
#' @return An xts or matrix object with columns: fastK, fastD, slowD.
#'
#' @author DengYishuo
#' @keywords ts momentum indicator
#'
#' @examples
#' data(ttrc)
#' stoch_values <- stoch(ttrc[, c("High", "Low", "Close")])
#' plot(tail(stoch_values[, "fastK"], 100),
#'   type = "l",
#'   main = "Fast %K of Stochastic Oscillator", ylab = "%K"
#' )
#'
#' @importFrom xts try.xts reclass
#'
#' @export

stoch <- function(HLC, nFastK = 14, nFastD = 3, nSlowD = 3,
                  maType, bounded = TRUE, smooth = 1, ...) {
  # Convert input to xts or matrix
  HLC <- try.xts(HLC, error = as.matrix)

  # Extract price components
  if (NCOL(HLC) == 3) {
    high <- HLC[, 1]
    low <- HLC[, 2]
    close <- HLC[, 3]
  } else if (NCOL(HLC) == 1) {
    high <- HLC
    low <- HLC
    close <- HLC
  } else {
    stop("Price series must be High-Low-Close or univariate")
  }

  # Calculate high and low ranges
  if (bounded) {
    hmax <- runMax(high, nFastK)
    lmin <- runMin(low, nFastK)
  } else {
    hmax <- runMax(c(high[1], high[-NROW(HLC)]), nFastK)
    lmin <- runMin(c(low[1], low[-NROW(HLC)]), nFastK)
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
  reclass(result, HLC)
}
