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
#' @title Calculate Triple Smoothed Exponential Oscillator
#' @description
#' The TRIX indicator calculates the rate of change of a triple exponential
#' moving average. Developed by Jack K. Hutson.
#' The TRIX is calculated as follows:\cr 3MA = \code{MA}( \code{MA}(
#' \code{MA}(\code{price}) ) )\cr \eqn{trix = 100 * [ 3MA(t) / 3MA(t - 1) - 1 ]}
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Number of periods for moving average. Default is 20.
#' @param nSig Number of periods for signal line moving average. Default is 9.
#' @param maType Either:
#'  \enumerate{
#'    \item A function or a string naming the function to be called.
#'    \item A \emph{list} with the first component like (1) above, and
#'      additional parameters specified as \emph{named} components.
#'      See Examples.
#'  }
#' @param percent logical; if \code{TRUE}, the rate of change is calculated
#' using the \code{ROC} function, otherwise the \code{momentum} function is
#' used. Default is \code{TRUE}.
#' @param append A logical value. If \code{TRUE}, the calculated TRIX and signal line
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' TRIX and signal line values will be returned. Defaults to \code{FALSE}.
#' @param ... Other arguments to be passed to the \code{maType} function in
#' case (1) above.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the TRIX and signal line values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated TRIX and signal line values appended, maintaining the integrity of the time - series
#' alignment.
#' @note Buy/sell signals are generated when the TRIX crosses above/below zero.
#' A nine - period EMA of the TRIX is used as a default signal line. Buy/sell
#' signals are generated when the TRIX crosses above/below the signal line and
#' is also above/below zero.
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://www.fmlabs.com/reference/default.htm?url=TRIX.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=114}\cr
#' \url{https://www.linnsoft.com/techind/trix - triple - smoothed - exponential - oscillator}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:trix}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' trix_result1 <- add_TRIX(TSLA)
#'
#' # Modifying n and without appending
#' trix_result2 <- add_TRIX(TSLA, n = 25)
#'
#' # Using default parameters and appending
#' trix_result3 <- add_TRIX(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' trix_result4 <- add_TRIX(TSLA, n = 25, append = TRUE)
#' }
add_TRIX <- function(OHLCV, n = 20, nSig = 9, maType, percent = TRUE, append = FALSE, ...) {
  # Assume we use Close price for calculation, can be adjusted
  price <- OHLCV[, "Close"]

  # Default MA
  if (missing(maType)) {
    maType <- "EMA"
  }

  # Case of different 'maType's for all MAs.
  if (is.list(maType)) {
    # Make sure maType is a list of lists
    maTypeInfo <- sapply(maType, is.list)
    if (!(all(maTypeInfo) && length(maTypeInfo) == 4)) {
      stop(
        "If \'maType\' is a list, you must specify\n ",
        "*four* MAs (see Examples section of ?TRIX)"
      )
    }

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with function's formal 'n'
    if (!is.null(formals(maType[[1]][[1]])$n) && is.null(maType[[1]]$n)) {
      maType[[1]]$n <- n
    }
    if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
      maType[[2]]$n <- n
    }
    if (!is.null(formals(maType[[3]][[1]])$n) && is.null(maType[[3]]$n)) {
      maType[[3]]$n <- n
    }
    if (!is.null(formals(maType[[4]][[1]])$n) && is.null(maType[[4]]$n)) {
      maType[[4]]$n <- nSig
    }

    mavg1 <- do.call(maType[[1]][[1]], c(list(price), maType[[1]][-1]))
    mavg2 <- do.call(maType[[2]][[1]], c(list(mavg1), maType[[2]][-1]))
    mavg3 <- do.call(maType[[3]][[1]], c(list(mavg2), maType[[3]][-1]))
  }

  # Case of one 'maType' for all MAs.
  else {
    mavg1 <- do.call(maType, c(list(price), list(n = n, ...)))
    mavg2 <- do.call(maType, c(list(mavg1), list(n = n, ...)))
    mavg3 <- do.call(maType, c(list(mavg2), list(n = n, ...)))
  }

  if (percent) {
    TRIX <- 100 * ROC(mavg3, n = 1, na.pad = TRUE, type = "discrete")
  } else {
    TRIX <- momentum(mavg3, n = 1, na.pad = TRUE)
  }

  if (is.list(maType)) {
    signal <- do.call(maType[[4]][[1]], c(list(TRIX), maType[[4]][-1]))
  } else {
    signal <- do.call(maType, c(list(TRIX), list(n = nSig, ...)))
  }

  result <- cbind(TRIX, signal)
  colnames(result) <- c("TRIX", "signal")

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, result)
    return(combined_result)
  } else {
    return(result)
  }
}
