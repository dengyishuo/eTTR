#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2020  Joshua M. Ulrich
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
#' @title Calculate Keltner Channels
#' @description
#' Keltner Channels are volatility - based envelopes set above and below a moving
#' average. This indicator is similar to Bollinger Bands, but Keltner Channels
#' use the Average True Range (ATR) to set channel distance.
#' Keltner Channels are a trend following indicator, and can also be used to
#' identify overbought and oversold levels when there is no trend.
#' Chester Keltner is credited with the original version of Keltner Channels in
#' his 1960 book. Linda Bradford Raschke introduced the newer version of
#' Keltner Channels in the 1980s.
#' @aliases add_KeltnerChannels
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume prices.
#' @param n Number of periods for moving average. Defaults to 20.
#' @param maType A function or a string naming the function to be called.
#' @param atr The number of average true range distances to apply. Defaults to 2.
#' @param append A logical value. If \code{TRUE}, the calculated Keltner Channels
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Keltner Channels values will be returned. Defaults to \code{FALSE}.
#' @param ... Other arguments to be passed to the maType function.
#' @section Details : Keltner Channels consist of three lines:
#' The middle band is generally a 20 - period EMA of the typical price
#' \code{[high + low + close]/3}. The upper and lower bands are multiples of average
#' true range (usually 2) above and below the MA.
#' The middle band is usually calculated using the typical price, but if a
#' univariate series (e.g. Close, Weighted Close, Median Price, etc.) is
#' provided, it will be used instead.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a matrix (if \code{try.xts} fails) containing the columns:
#'  \describe{
#'     \item{dn}{ The lower Keltner Channel. }
#'     \item{mavg}{ The middle moving average. }
#'     \item{up}{ The upper Keltner Channel. }
#'  }
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Keltner Channels values appended, maintaining the integrity of the time - series
#' alignment.
#' @keywords ts
#' @author Nick Procyk, Joshua Ulrich
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:keltner_channels}\cr
#' \url{https://www.linnsoft.com/techind/keltner - channels - keltu - keltd}\cr
#' \url{https://www.investopedia.com/terms/k/keltnerchannel.asp}\cr
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.
#' @importFrom xts xcoredata
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' kc_result1 <- add_keltnerChannels(TSLA)
#'
#' # Modifying n and without appending
#' kc_result2 <- add_keltnerChannels(TSLA, n = 30)
#'
#' # Using default parameters and appending
#' kc_result3 <- add_keltnerChannels(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' kc_result4 <- add_keltnerChannels(TSLA, n = 30, append = TRUE)
#' }
#' @export
add_keltnerChannels <- function(OHLCV, n = 20, maType, atr = 2, append = FALSE, ...) {
  # Extract HLC from OHLCV
  HLC <- OHLCV[, c("High", "Low", "Close")]

  atrHLC <- HLC
  HLC <- try.xts(HLC, error = as.matrix)
  if (NCOL(HLC) == 3) {
    if (is.xts(HLC)) {
      xa <- xts::xcoredata(HLC)
      HLC <- xts(apply(HLC, 1, mean), index(HLC))
      xts::xcoredata(HLC) <- xa
    } else {
      HLC <- apply(HLC, 1, mean)
    }
  } else if (NCOL(HLC) != 1) {
    stop("Price series must be either High - Low - Close, or Close/univariate.")
  }
  maArgs <- list(n = n, ...)
  if (missing(maType)) {
    maType <- "EMA"
  }
  mavg <- do.call(maType, c(list(HLC), maArgs))
  avgtruerange <- ATR(atrHLC, n = n)

  up <- mavg + atr * avgtruerange[, 2]
  dn <- mavg - atr * avgtruerange[, 2]

  res <- cbind(dn, mavg, up)
  colnames(res) <- c("dn", "mavg", "up")
  res <- reclass(res, HLC)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, res)
    return(combined_result)
  } else {
    return(res)
  }
}
