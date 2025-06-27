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
#' @title Calculate Relative Strength Index
#' @description
#' The Relative Strength Index (RSI) calculates a ratio of the recent upward
#' price movements to the absolute price movement. Developed by J. Welles
#' Wilder.
#' The RSI calculation is \code{RSI = 100 - 100 / ( 1 + RS )}, where \code{RS}
#' is the smoothed ratio of 'average' gains over 'average' losses. The
#' 'averages' aren't true averages, since they're divided by the value of
#' \code{n} and not the number of periods in which there are  gains/losses.
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Number of periods for moving averages. Defaults to 14.
#' @param maType Either:
#'  \enumerate{
#'    \item A function or a string naming the function to be called.
#'    \item A \emph{list} with the first component like (1) above, and
#'      additional parameters specified as \emph{named} components.
#'      See Examples.
#'  }
#' @param append A logical value. If \code{TRUE}, the calculated Relative Strength Index
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Relative Strength Index values will be returned. Defaults to \code{FALSE}.
#' @param ... Other arguments to be passed to the \code{maType} function in
#' case (1) above.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the RSI values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Relative Strength Index values appended, maintaining the integrity of the time - series
#' alignment.
#' @note The RSI is usually interpreted as an overbought/oversold (over 70 /
#' below 30) indicator. Divergence with price may also be useful. For example,
#' if price is making new highs/lows, but RSI is not, it could indicate a
#' reversal.
#' You can calculate a stochastic RSI by using the function \code{\link{stoch}}
#' on RSI values.
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section. See \code{\link{CMO}} for a variation on
#' RSI.
#' @references The following site(s) were used to code/document this
#' indicator:
#' \cr Relative Strength Index:\cr
#' \url{https://www.fmlabs.com/reference/RSI.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=100}\cr
#' \url{https://www.linnsoft.com/techind/relative - strength - index - rsi}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:relative_strength_index_rsi}\cr
#' \cr Stochastic RSI:\cr
#' \url{https://www.fmlabs.com/reference/StochRSI.htm}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:stochrsi}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' rsi_result1 <- add_RSI(TSLA)
#'
#' # Modifying n and without appending
#' rsi_result2 <- add_RSI(TSLA, n = 20)
#'
#' # Using default parameters and appending
#' rsi_result3 <- add_RSI(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' rsi_result4 <- add_RSI(TSLA, n = 20, append = TRUE)
#' }
add_RSI <- function(OHLCV, n = 14, maType, append = FALSE, ...) {
  # Assume we use Close price for calculation, can be adjusted
  price <- OHLCV[, "Close"]
  price <- try.xts(price, error = as.matrix)

  # Calculate price momentum
  up <- momentum(price, n = 1, na.pad = TRUE)
  which.dn <- which(up < 0)
  dn <- up * 0
  dn[which.dn] <- -up[which.dn]
  up[which.dn] <- 0

  maArgs <- list(n = n, ...)
  # Default Welles Wilder EMA
  if (missing(maType)) {
    maType <- "EMA"
    if (is.null(maArgs$wilder)) {
      # do not overwrite user-provided value
      maArgs$wilder <- TRUE
    }
  }

  # Case of two different 'maType's for both MAs.
  if (is.list(maType)) {
    # Ensure maType is a list of lists with exactly 2 elements
    if (!all(sapply(maType, is.list)) || length(maType) != 2) {
      stop("If'maType' is a list, you must specify *two* MAs (see Examples section of?RSI)")
    }
    # Check if 'n' is specified for each MA function in the list
    for (i in 1:2) {
      if (!is.null(formals(maType[[i]][[1]])$n) && is.null(maType[[i]]$n)) {
        maType[[i]]$n <- n
      }
    }
    mavgUp <- do.call(maType[[1]][[1]], c(list(up), maType[[1]][-1]))
    mavgDn <- do.call(maType[[2]][[1]], c(list(dn), maType[[2]][-1]))
  }
  # Case of one 'maType' for both MAs.
  else {
    mavgUp <- do.call(maType, c(list(up), maArgs))
    mavgDn <- do.call(maType, c(list(dn), maArgs))
  }

  rsi <- 100 * mavgUp / (mavgUp + mavgDn)

  if (!is.null(dim(rsi)) && ncol(rsi) == 1L) {
    colnames(rsi) <- "rsi"
  }

  rsi <- reclass(rsi, price)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, rsi)
    return(combined_result)
  } else {
    return(rsi)
  }
}
