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
#' @title Construct (optionally further smoothed and centered ) volatility bands around prices
#' @description
#' John Bollinger's famous adaptive volatility bands most often use the typical
#' price of an HLC series, or may be calculated on a univariate price series
#' (see \code{\link{BBands}}).
#' This function applies a second moving average denoted by \code{fastn} to
#' filter out higher - frequency noise, making the bands somewhat more stable to
#' temporary fluctuations and spikes.
#' If \code{centered} is \code{TRUE}, the function also further smoothes and
#' centers the bands around a centerline adjusted to remove this higher
#' frequency noise.  If \code{lavg} is also \code{TRUE}, the smoothing applied
#' for the middle band (but not the volatility bands) is doubled to further
#' smooth the price - response function.
#' If you have multiple different price series in \code{prices}, and want to use
#' this function, call this functions using \code{lapply(prices,PBands,...)}.
#' @aliases add_pbands add_priceBands
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Number of periods to average over. Defaults to 20.
#' @param maType A function or a string naming the function to be called. Defaults to "SMA".
#' @param sd The number of standard deviations to use. Defaults to 2.
#' @param fastn Number of periods to use for smoothing higher - frequency 'noise'. Defaults to 2.
#' @param centered Whether to center the bands around a series adjusted for high
#'  frequency noise, default \code{FALSE}.
#' @param lavg Whether to use a longer \code{(n*2)} smoothing period for
#'  centering, default \code{FALSE}.
#' @param append A logical value. If \code{TRUE}, the calculated price volatility bands
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' price volatility bands values will be returned. Defaults to \code{FALSE}.
#' @param ... any other pass - thru parameters, usually for function named by
#'  \code{maType}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a matrix (if \code{try.xts} fails) containing the columns:
#'  \describe{
#'     \item{ dn }{ The lower price volatility Band. }
#'     \item{ center }{ The smoothed centerline (see details). }
#'     \item{ up }{ The upper price volatility Band. }
#'  }
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated price volatility bands values appended, maintaining the integrity of the time - series
#' alignment.
#' @author Brian G. Peterson
#' @seealso \code{\link{BBands}}
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' pbands_result1 <- add_PBands(TSLA)
#'
#' # Modifying n and without appending
#' pbands_result2 <- add_PBands(TSLA, n = 25)
#'
#' # Using default parameters and appending
#' pbands_result3 <- add_PBands(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' pbands_result4 <- add_PBands(TSLA, n = 25, append = TRUE)
#' }
add_PBands <- function(
    OHLCV, n = 20, maType = "SMA", sd = 2, fastn = 2,
    centered = FALSE, lavg = FALSE, append = FALSE, ...) {
  # Assume we use Close price for calculation, can be adjusted
  prices <- OHLCV[, "Close"]

  if (!is.vector(prices) && ncol(prices) > 1) {
    stop(
      "prices should be a univariate series, maybe use",
      "lapply(prices,PBands) instead?"
    )
  }

  prices <- try.xts(prices, error = as.matrix)

  # Default MA
  if (missing(maType)) {
    maType <- "SMA"
  }

  maArgs <- list(n = n, ...)
  mavg <- do.call(maType, c(list(prices), maArgs))

  maFastArgs <- list(n = fastn, ...)
  fastmavg <- do.call(maType, c(list(prices), maFastArgs))

  sdev <- runSD((mavg - fastmavg), n = n, sample = FALSE)

  if (!isTRUE(centered)) {
    center <- mavg
  } else {
    centerrun <- (mavg - fastmavg) / sdev
    if (isTRUE(lavg)) {
      maArgs <- list(n = (n * 2), ...)
    }
    center <- mavg + (do.call(maType, c(list(centerrun), maArgs)))
  }

  up <- center + sd * sdev
  dn <- center - sd * sdev

  res <- cbind(dn, center, up)
  colnames(res) <- c("dn", "center", "up")

  res <- reclass(res, prices)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, res)
    return(combined_result)
  } else {
    return(res)
  }
}
