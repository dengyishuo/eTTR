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
#' @title Calculate Ehler's Correlation Trend Indicator
#' @description
#' Ehler's Correlation Trend Indicator (CTI) measures the Spearman correlation
#' of the price with the ideal trend line: a straight line with increasing
#' slope.
#' The CTI measures the Spearman correlation between the price and the ideal
#' trend line with slope of \code{slope}, over the past \code{n} days.
#' See URL in references section for further details.
#' @param OHLCV Price series that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Number of periods to use. Defaults to 20.
#' @param slope Slope of desired trend. Defaults to 1.
#' @param append A logical value. If \code{TRUE}, the calculated Ehler's Correlation Trend Indicator
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Ehler's Correlation Trend Indicator values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a matrix (if \code{try.xts} fails) with the column:
#'  \describe{
#'   \item{cti}{ The Correlation Trend Indicator. }
#'  }
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Ehler's Correlation Trend Indicator values appended, maintaining the integrity of the time - series
#' alignment.
#' @note Positive/negative CTI values signal positive/negative correlation with
#' the desired trend line slope. A simple strategy could be long when the CTI
#' is positive and, short when it is negative.
#' @author Ethan Smith, Joshua Ulrich
#' @seealso See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{ADX}},
#' \code{\link{VHF}}, \code{\link{GMMA}}, \code{\link{TDI}} for other
#' indicators that measure trend direction/strength.
#' @references
#' John Ehlers, Correlation Trend Indicator, Stocks & Commodities May - 2020
#' The following site(s) were used to code/document this indicator:\cr
#' \url{https://financial - hacker.com/petra - on - programming - a - unique - trend - indicator/}\cr
#' @keywords ts
#' @importFrom zoo rollapplyr
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' cti_result1 <- add_CTI(TSLA)
#'
#' # Modifying n and without appending
#' cti_result2 <- add_CTI(TSLA, n = 30)
#'
#' # Using default parameters and appending
#' cti_result3 <- add_CTI(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' cti_result4 <- add_CTI(TSLA, n = 30, append = TRUE)
#' }
add_CTI <- function(OHLCV, n = 20, slope = 1, append = FALSE) {
  # Assume we use Close price for calculation, can be adjusted
  price <- OHLCV[, "Close"]

  x <- try.xts(price, error = as.matrix)
  y <- slope * seq_along(x)

  f <- function(.) {
    stats::cor(.[, 1], .[, 2], method = "spearman")
  }

  cti <- zoo::rollapplyr(cbind(x, y), n, f, by.column = FALSE, fill = NA)

  if (!is.null(dim(cti))) {
    colnames(cti) <- "cti"
  }

  cti <- reclass(cti, x)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, cti)
    return(combined_result)
  } else {
    return(cti)
  }
}
