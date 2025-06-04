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
#'
#' @title Stochastic Momentum Index
#' @description Calculates the SMI value and its signal line.
#'
#' @param HLC Object coercible to xts or matrix, containing High-Low-Close prices
#' or a univariate series.
#' @param n Number of periods for range calculation (default: 13).
#' @param nFast Fast moving average periods (default: 2).
#' @param nSlow Slow moving average periods (default: 25).
#' @param nSig Signal line periods (default: 9).
#' @param maType Moving average type: function, string, or list of configurations
#' (default: "EMA").
#' @param bounded Logical, use current period in range calculation (default: TRUE).
#' @param ... Additional arguments passed to moving average functions.
#'
#' @return An xts or matrix object with columns: SMI, signal.
#'
#' @author DengYishuo
#' @keywords ts momentum indicator
#'
#' @examples
#' data(TSLA)
#' smi_values <- SMI(TSLA[, c("High", "Low", "Close")])
#' plot(tail(smi_values[, "SMI"], 100),
#'   type = "l",
#'   main = "Stochastic Momentum Index", ylab = "SMI"
#' )
#' abline(h = c(20, 80), col = "gray", lty = 2)
#'
#' @importFrom xts try.xts reclass
#'
#' @export

SMI <- function(HLC, n = 13, nFast = 2, nSlow = 25, nSig = 9,
                maType, bounded = TRUE, ...) {
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
    hmax <- runMax(high, n)
    lmin <- runMin(low, n)
  } else {
    hmax <- runMax(c(high[1], high[-NROW(HLC)]), n)
    lmin <- runMin(c(low[1], low[-NROW(HLC)]), n)
  }

  # Handle NA values in initial ranges
  hmax <- ifelse(is.na(hmax), high, hmax)
  lmin <- ifelse(is.na(lmin), low, lmin)

  # Calculate SMI components
  HLdiff <- hmax - lmin
  Cdiff <- close - (hmax + lmin) / 2

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
    num1 <- do.call(maType[[1]][[1]], c(list(Cdiff), maType[[1]][-1]))
    den1 <- do.call(maType[[1]][[1]], c(list(HLdiff), maType[[1]][-1]))
    num2 <- do.call(maType[[2]][[1]], c(list(num1), maType[[2]][-1]))
    den2 <- do.call(maType[[2]][[1]], c(list(den1), maType[[2]][-1]))

    SMI <- 100 * (num2 / (den2 / 2))
    signal <- do.call(maType[[3]][[1]], c(list(SMI), maType[[3]][-1]))
  } else {
    # Single moving average type
    num1 <- do.call(maType, c(list(Cdiff), list(n = nSlow, ...)))
    den1 <- do.call(maType, c(list(HLdiff), list(n = nSlow, ...)))
    num2 <- do.call(maType, c(list(num1), list(n = nFast, ...)))
    den2 <- do.call(maType, c(list(den1), list(n = nFast, ...)))

    SMI <- 100 * (num2 / (den2 / 2))
    signal <- do.call(maType, c(list(SMI), list(n = nSig, ...)))
  }

  # Construct result
  result <- cbind(SMI, signal)
  colnames(result) <- c("SMI", "signal")
  reclass(result, HLC)
}
