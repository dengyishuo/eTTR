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
#' @title Calculate MACD Oscillator
#' @description
#' The MACD was developed by Gerald Appel and is probably the most popular price
#' oscillator.  The MACD function documented in this page compares a fast moving
#' average (MA) of a series with a slow MA of the same series.  It can be used
#' as a generic oscillator for any univariate series, not only price.
#' The MACD function either subtracts the fast MA from the slow MA, or finds the
#' rate of change between the fast MA and the slow MA.
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param nFast Number of periods for fast moving average. Defaults to 12.
#' @param nSlow Number of periods for slow moving average. Defaults to 26.
#' @param nSig Number of periods for signal moving average. Defaults to 9.
#' @param maType Either:
#'  \enumerate{
#'    \item A function or a string naming the function to be called.
#'    \item A \emph{list} with the first component like (1) above, and
#'      additional parameters specified as \emph{named} components.
#'      See Examples.
#'  }
#' @param percent logical; if \code{TRUE}, the percentage difference between the
#' fast and slow moving averages is returned, otherwise the difference between
#' the respective averages is returned. Defaults to TRUE.
#' @param append A logical value. If \code{TRUE}, the calculated MACD values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' MACD values will be returned. Defaults to \code{FALSE}.
#' @param ... Other arguments to be passed to the \code{maType} function in
#' case (1) above.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a matrix (if \code{try.xts} fails) containing the columns:
#'  \describe{
#'   \item{ macd }{ The price (volume, etc.) oscillator. }
#'   \item{ signal }{ The oscillator signal line (a moving average of the oscillator). }
#'  }
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated MACD values appended, maintaining the integrity of the time - series
#' alignment.
#' @note The MACD is a special case of the general oscillator applied to price.
#' The MACD can be used as a general oscillator applied to any series. Time
#' periods for the MACD are often given as 26 and 12, but the original formula
#' used exponential constants of 0.075 and 0.15, which are closer to
#' 25.6667 and 12.3333 periods.
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.
#' @references The following site(s) were used to code/document this
#' indicator:
#' \cr Moving Average Convergence/Divergence (MACD):\cr
#' \url{https://www.fmlabs.com/reference/MACD.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=66}\cr
#' \url{https://www.linnsoft.com/techind/macd}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:moving_average_convergence_divergence_macd}\cr
#' \cr Price Oscillator:\cr
#' \url{https://www.fmlabs.com/reference/PriceOscillator.htm}\cr
#' \url{https://www.fmlabs.com/reference/PriceOscillatorPct.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=94}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:price_oscillators_ppo}\cr
#' \cr Volume Oscillator:\cr
#' \url{https://www.fmlabs.com/reference/PVO.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=122}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' macd_result1 <- add_MACD(TSLA)
#'
#' # Modifying nFast and without appending
#' macd_result2 <- add_MACD(TSLA, nFast = 14)
#'
#' # Using default parameters and appending
#' macd_result3 <- add_MACD(TSLA, append = TRUE)
#'
#' # Modifying nFast and appending
#' macd_result4 <- add_MACD(TSLA, nFast = 14, append = TRUE)
#' }
add_MACD <- function(OHLCV, nFast = 12, nSlow = 26, nSig = 9, maType, percent = TRUE, append = FALSE, ...) {
  # Assume we use Close price for calculation, can be adjusted
  x <- OHLCV[, "Close"]
  # Try to convert x to xts, if fails, convert to matrix
  x <- try.xts(x, error = as.matrix)

  # Default MA
  if (missing(maType)) {
    maType <- "EMA"
  }

  # Case of two different 'maType's for both MAs.
  if (is.list(maType)) {
    # Ensure maType is a list of lists with exactly 3 elements
    if (!all(sapply(maType, is.list)) || length(maType) != 3) {
      stop("If'maType' is a list, you must specify *three* MAs (see Examples section of?MACD)")
    }

    # Populate 'n' argument for each MA function in the list
    if (!is.null(formals(maType[[1]][[1]])$n) && is.null(maType[[1]]$n)) {
      maType[[1]]$n <- nFast
    }
    if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
      maType[[2]]$n <- nSlow
    }
    if (!is.null(formals(maType[[3]][[1]])$n) && is.null(maType[[3]]$n)) {
      maType[[3]]$n <- nSig
    }

    mavg.fast <- do.call(maType[[1]][[1]], c(list(x), maType[[1]][-1]))
    mavg.slow <- do.call(maType[[2]][[1]], c(list(x), maType[[2]][-1]))
  }
  # Case of one 'maType' for both MAs.
  else {
    mavg.fast <- do.call(maType, c(list(x), list(n = nFast, ...)))
    mavg.slow <- do.call(maType, c(list(x), list(n = nSlow, ...)))
  }

  if (percent) {
    macd <- 100 * (mavg.fast / mavg.slow - 1)
  } else {
    macd <- mavg.fast - mavg.slow
  }

  if (is.list(maType)) {
    signal <- do.call(maType[[3]][[1]], c(list(macd), maType[[3]][-1]))
  } else {
    signal <- do.call(maType, c(list(macd), list(n = nSig, ...)))
  }

  result <- cbind(macd, signal)
  colnames(result) <- c("macd", "signal")
  result <- reclass(result, x)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, result)
    return(combined_result)
  } else {
    return(result)
  }
}
