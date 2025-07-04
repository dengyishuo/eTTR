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
#' @title Calculate Guppy Multiple Moving Averages
#' @description
#' Calculate the Guppy Multiple Moving Average of a series.
#' The Guppy Multiple Moving Average signals a changing trend when the
#' \code{short} and \code{long} groups of moving averages intersect.  An up/down
#' trend exists when the short/long - term moving averages are greater than the
#' long/short - term averages.
#' @aliases ADD_GMMA
#' @param OHLCV Price, volume, etc. series that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param short Vector of short - term periods. Defaults to c(3, 5, 8, 10, 12, 15).
#' @param long Vector of long - term periods. Defaults to c(30, 35, 40, 45, 50, 60).
#' @param maType Either:
#'  \enumerate{
#'    \item A function or a string naming the function to be called.
#'    \item A \emph{list} with the first component like (1) above, and
#'      additional parameters specified as \emph{named} components.
#'      See Examples.
#'  }
#' @param append A logical value. If \code{TRUE}, the calculated Guppy Multiple Moving Average
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Guppy Multiple Moving Average values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the Guppy Multiple Moving Average.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Guppy Multiple Moving Average values appended, maintaining the integrity of the time - series
#' alignment.
#' @author DengYishuo
#' @seealso See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{ADX}},
#' \code{\link{VHF}}, \code{\link{TDI}} for other indicators that measure trend
#' direction/strength.
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://www.investopedia.com/terms/g/guppy - multiple - moving - average.asp}\cr
#' @keywords ts
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' gmma_result1 <- add_GMMA(TSLA)
#'
#' # Using default parameters and appending
#' gmma_result2 <- add_GMMA(TSLA, append = TRUE)
#' }
#' @export
add_GMMA <- function(OHLCV, short = c(3, 5, 8, 10, 12, 15), long = c(30, 35, 40, 45, 50, 60), maType, append = FALSE) {
  # Assume we use Close price for calculation, can be adjusted
  x <- OHLCV[, "Close"]

  x <- try.xts(x, error = as.matrix)

  # Default MA
  if (missing(maType)) {
    maType <- "EMA"
  }

  fn <- function(g) {
    do.call(maType, list(x, n = g))
  }
  gmma <- do.call(cbind, lapply(c(short, long), fn))
  colnames(gmma) <- c(paste("short lag", short), paste("long lag", long))

  gmma <- reclass(gmma, x)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, gmma)
    return(combined_result)
  } else {
    return(gmma)
  }
}
