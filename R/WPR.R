#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2025-2030  DengYishuo
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
#' @title Williams %R
#' @description
#' Measures closing price's position relative to price range over last `n` periods.
#' @param HLC Object coercible to xts/matrix (High-Low-Close or univariate).
#' @param n Lookback period (default 14).
#' @param scale Scale to \code{[0, -100]} range? (default FALSE).
#' @return
#' Object matching `HLC` class containing Williams %R values.
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
#' data(TSLA)
#' hlc <- TSLA[, c("High", "Low", "Close")]
#' wpr <- WPR(hlc)
#' wpr_scaled <- WPR(hlc, scale = TRUE)
WPR <- function(HLC, n = 14, scale = FALSE) {
  # Original function implementation remains unchanged
  HLC <- try.xts(HLC, error = as.matrix)
  if (NCOL(HLC) == 3) {
    high <- HLC[, 1]
    low <- HLC[, 2]
    close <- HLC[, 3]
  } else if (NCOL(HLC) == 1) {
    high <- low <- close <- HLC
  } else {
    stop("Price series must be either High-Low-Close, or Close")
  }

  hmax <- runMax(high, n)
  lmin <- runMin(low, n)
  pctR <- (hmax - close) / (hmax - lmin)
  pctR[is.nan(pctR)] <- 0.5

  if (scale) pctR <- -100 * pctR
  reclass(pctR, HLC)
}
