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
#' @title Calculate Donchian Channel
#' @description
#' Donchian Channels were created by Richard Donchian and were used to generate
#' buy and sell signals for the Turtle Trading system. Donchian Channels consist
#' of two (sometimes three) lines: The top line is the highest high of the past
#' \code{n} periods.  The bottom line is the lowest low of the past \code{n}
#' periods.  The middle line is the average of the top and bottom lines.
#' @aliases ADD_DonchianChannel add_Donchian
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume prices.
#' @param n Number of periods for moving average. Defaults to 10.
#' @param include.lag Should values be lagged so that today's prices are not
#' included in the calculation? See Note. Defaults to FALSE.
#' @param append A logical value. If \code{TRUE}, the calculated Donchian Channel
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Donchian Channel values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a matrix (if \code{try.xts} fails) containing the columns:
#'  \describe{
#'   \item{ high }{ The highest high series. }
#'   \item{ mid }{ The average of \code{high} and \code{low}. }
#'   \item{ low }{ The lowest low series. }
#'  }
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Donchian Channel values appended, maintaining the integrity of the time - series
#' alignment.
#' @note The default of \code{include.lag = FALSE} makes \code{DonchainChannel}
#' consistent with other \pkg{eTTR} functions, in that it includes the current
#' period in the calculation.
#' The default is different than the original calculation, which would calculate
#' the indicator using periods t - 1 through t - n. Setting \code{include.lag = TRUE}
#' will return the result of the original calculation. The default of this
#' argument may change in the future.
#' @author DengYishuo
#' @seealso See \code{\link{BBands}}.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.linnsoft.com/techind/donchian - channels}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' dc_result1 <- add_DonchianChannel(TSLA)
#'
#' # Modifying n and without appending
#' dc_result2 <- add_DonchianChannel(TSLA, n = 15)
#'
#' # Using default parameters and appending
#' dc_result3 <- add_DonchianChannel(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' dc_result4 <- add_DonchianChannel(TSLA, n = 15, append = TRUE)
#' }
add_DonchianChannel <- function(OHLCV, n = 10, include.lag = FALSE, append = FALSE) {
  # Extract HL from OHLCV
  hl <- OHLCV[, c("High", "Low")]

  hl <- try.xts(hl, error = as.matrix)

  if (!(NCOL(hl) %in% c(1, 2))) {
    stop("Price series must be either High - Low, or Close/univariate.")
  }
  if (NCOL(hl) == 2) {
    hi <- hl[, 1]
    lo <- hl[, 2]
  } else {
    hi <- hl
    lo <- hl
  }

  high <- runMax(hi, n)
  low <- runMin(lo, n)
  mid <- (high + low) / 2

  result <- cbind(high, mid, low)
  colnames(result) <- c("high", "mid", "low")

  if (include.lag) {
    # use lag.xts in case 'result' is a matrix
    result <- lag.xts(result)
  }

  result <- reclass(result, hl)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, result)
    return(combined_result)
  } else {
    return(result)
  }
}
