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
#' @title Calculate Bollinger Bands
#' @description
#' Calculates Bollinger Bands, a volatility indicator comparing price levels over time.
#' @param OHLCV Object coercible to xts/matrix containing Open - High - Low - Close - Volume prices.
#' @param n Number of periods for moving average (default 20).
#' @param maType Function or string naming moving average type.
#' @param sd Number of standard deviations (default 2).
#' @param append A logical value. If \code{TRUE}, the calculated result columns
#' ("dn", "mavg", "up", "pctB") will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' result data will be returned. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to `maType`.
#' @return
#' If \code{append = FALSE}, an object matching \code{OHLCV} class with columns:
#' - `dn`: Lower band
#' - `mavg`: Moving average
#' - `up`: Upper band
#' - `pctB`: %B value
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated columns appended, maintaining the integrity of the time - series
#' alignment.
#' @note
#' Non - SMA averages cause inconsistencies since SD calculations assume SMA.
#' @details
#' Calculates three bands:
#' - Middle: SMA of typical price \eqn{(high + low + close)/3}
#' - Upper: `sd` standard deviations above MA
#' - Lower: `sd` standard deviations below MA
#' Uses univariate series directly if provided.
#' @references
#' - \url{https://www.fmlabs.com/reference/Bollinger.htm}
#' - \url{https://school.stockcharts.com/doku.php?id=technical_indicators:bollinger_bands}
#' @seealso
#' Moving average functions: [eTTR::SMA()], [eTTR::EMA()]
#' @keywords ts
#' @importFrom xts xcoredata
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' bbands_result1 <- add_BBands(TSLA)
#'
#' # Modifying n and without appending
#' bbands_result2 <- add_BBands(TSLA, n = 30)
#'
#' # Using default parameters and appending
#' bbands_result3 <- add_BBands(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' bbands_result4 <- add_BBands(TSLA, n = 30, append = TRUE)
#' }
add_BBands <- function(OHLCV, n = 20, maType, sd = 2, append = FALSE, ...) {
  # Extract HLC from OHLCV
  hlc <- OHLCV[, c("High", "Low", "Close")]

  hlc <- try.xts(hlc, error = as.matrix)
  if (NCOL(hlc) == 3) {
    if (is.xts(hlc)) {
      xa <- xts::xcoredata(hlc)
      hlc <- xts(apply(hlc, 1, mean), index(hlc))
      xts::xcoredata(hlc) <- xa
    } else {
      hlc <- apply(hlc, 1, mean)
    }
  } else if (NCOL(hlc) != 1) {
    stop("Price series must be either High - Low - Close, or Close/univariate.")
  }

  ma_args <- list(n = n, ...)
  if (missing(maType)) {
    maType <- "SMA"
  }

  mavg <- do.call(maType, c(list(hlc), ma_args))
  sdev <- runSD(hlc, n, sample = FALSE)

  up <- mavg + sd * sdev
  dn <- mavg - sd * sdev
  pctb <- (hlc - dn) / (up - dn)

  res <- cbind(dn, mavg, up, pctb)
  colnames(res) <- c("dn", "mavg", "up", "pctB")
  res <- reclass(res, hlc)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, res)
    return(combined_result)
  } else {
    return(res)
  }
}
