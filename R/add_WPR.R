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
#' @title Calculate Williams %R
#' @description
#' Measures closing price's position relative to price range over last `n` periods.
#' @param OHLCV Object coercible to xts/matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Lookback period (default 14).
#' @param scale Scale to \code{[0, -100]} range? (default FALSE).
#' @param append A logical value. If \code{TRUE}, the calculated Williams %R values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Williams %R values will be returned. Defaults to \code{FALSE}.
#' @return
#' If \code{append = FALSE}, an object matching \code{OHLCV} class containing Williams %R values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Williams %R values appended, maintaining the integrity of the time - series
#' alignment.
#' @details
#' \deqn{WPR = \frac{\mathit{highest\ high} - close}{\mathit{highest\ high} - \mathit{lowest\ low}}}
#' Equivalent to \eqn{1 - fastK} from stochastic oscillator.
#' @note Requires `runMax`/`runMin` from eTTR/quantmod.
#' @references
#' - \url{https://www.fmlabs.com/reference/WilliamsR.htm}
#' - \url{https://school.stockcharts.com/doku.php?id=technical_indicators:williams_r}
#' @seealso Related function: [eTTR::stoch()]
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' ohlcv <- TSLA[, c("Open", "High", "Low", "Close")]
#' # Using default parameters without appending
#' wpr_result1 <- add_WPR(ohlcv)
#'
#' # Using default parameters and appending
#' wpr_result2 <- add_WPR(ohlcv, append = TRUE)
#'
#' # Scaling and without appending
#' wpr_scaled_result1 <- add_WPR(ohlcv, scale = TRUE)
#'
#' # Scaling and appending
#' wpr_scaled_result2 <- add_WPR(ohlcv, scale = TRUE, append = TRUE)
#' }
add_WPR <- function(OHLCV, n = 14, scale = FALSE, append = FALSE) {
  # Assume we use High - Low - Close prices for calculation, can be adjusted
  if (NCOL(OHLCV) == 1) {
    HLC <- OHLCV
  } else if (NCOL(OHLCV) >= 3) {
    HLC <- OHLCV[, c("High", "Low", "Close")]
  } else {
    stop("Price series must have at least 1 (Close) or 3 (High - Low - Close) columns")
  }

  HLC <- try.xts(HLC, error = as.matrix)

  if (NCOL(HLC) == 3) {
    high <- HLC[, 1]
    low <- HLC[, 2]
    close <- HLC[, 3]
  } else if (NCOL(HLC) == 1) {
    high <- low <- close <- HLC
  }

  hmax <- runMax(high, n)
  lmin <- runMin(low, n)
  pctR <- (hmax - close) / (hmax - lmin)
  pctR[is.nan(pctR)] <- 0.5

  if (scale) pctR <- -100 * pctR

  pctR <- reclass(pctR, HLC)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    colname <- if (scale) "WPR_scaled" else "WPR"
    combined_result <- cbind(ohlcv, setNames(pctR, colname))
    return(combined_result)
  } else {
    return(pctR)
  }
}
