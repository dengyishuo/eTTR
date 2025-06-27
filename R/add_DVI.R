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
#' @title Calculate DV Intermediate Oscillator
#' @description
#' The DV Intermediate oscillator (DVI) is a very smooth momentum oscillator
#' that can also be used as a trend indicator.  Created by David Varadi. The DVI
#' combines smoothed returns over different time windows and the relative number
#'  of up versus down days (stretch) over different time windows.
#' @param OHLCV Price series that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Number of periods for the percent rank. Defaults to 252.
#' @param wts The weight given to the smoothed returns (magnitude) component and
#' the up/down days (stretch) component, respectively. Defaults to c(0.8, 0.2).
#' @param smooth The number of periods to smooth price. Defaults to 3.
#' @param magnitude A set of 3 periods used to smooth magnitude. Defaults to c(5, 100, 5).
#' @param stretch A set of 3 periods used to smooth stretch. Defaults to c(10, 100, 2).
#' @param exact.multiplier The weight applied to identical values in the window.
#' See \code{runPercentRank}. Defaults to 1.
#' @param append A logical value. If \code{TRUE}, the calculated DV Intermediate Oscillator
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' DV Intermediate Oscillator values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the DVI values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated DV Intermediate Oscillator values appended, maintaining the integrity of the time - series
#' alignment.
#' @author DengYishuo
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://cssanalytics.wordpress.com/2009/12/13/what - is - the - dvi/}\cr
#' \url{https://marketsci.wordpress.com/2010/07/27/css - analytics\%E2\%80\%99 - dvi - indicator - revealed/}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' dvi_result1 <- add_DVI(TSLA)
#'
#' # Modifying n and without appending
#' dvi_result2 <- add_DVI(TSLA, n = 300)
#'
#' # Using default parameters and appending
#' dvi_result3 <- add_DVI(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' dvi_result4 <- add_DVI(TSLA, n = 300, append = TRUE)
#' }
add_DVI <- function(
    OHLCV, n = 252, wts = c(0.8, 0.2), smooth = 3,
    magnitude = c(5, 100, 5), stretch = c(10, 100, 2), exact.multiplier = 1, append = FALSE) {
  # Assume we use Close price for calculation, can be adjusted
  price <- OHLCV[, "Close"]

  # try to convert 'price' to xts
  price <- try.xts(price, error = as.matrix)

  # ensure magnitude + stretch = 1
  wts_sum <- sum(wts)
  wts[1] <- wts[1] / wts_sum
  wts[2] <- wts[2] / wts_sum

  # calculate magnitude, based on average price return
  r <- price / SMA(price, smooth) - 1
  mag <- SMA((SMA(r, magnitude[1]) + SMA(r, magnitude[2]) / 10) / 2, magnitude[3])

  # calculate stretch, based on whether return is +/-
  b <- ifelse(price > lag.xts(price), 1, -1)
  str <- SMA((runSum(b, stretch[1]) + runSum(b, stretch[2]) / 10) / 2, stretch[3])


  # calculate the DVI magnitude and stretch for each period
  dvi_mag <- runPercentRank(mag, n, FALSE, exact.multiplier)
  dvi_str <- runPercentRank(str, n, FALSE, exact.multiplier)

  # calculate final DVI value
  dvi <- wts[1] * dvi_mag + wts[2] * dvi_str

  result <- cbind(dvi_mag, dvi_str, dvi)
  colnames(result) <- c("dvi.mag", "dvi.str", "dvi")

  # convert final DVI, magnitude, and stretch back to
  # original class of 'price'
  result <- reclass(result, price)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, result)
    return(combined_result)
  } else {
    return(result)
  }
}
