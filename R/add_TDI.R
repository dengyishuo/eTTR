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
#' @title Calculate Trend Detection Index
#' @description
#' The Trend Detection Index (TDI) attempts to identify starting and ending
#' trends. Developed by M. H. Pee.
#' The TDI is the (1) absolute value of the \code{n}-day sum of the \code{n}-day
#' momentum, minus the quantity of (2) \code{multiple}*\code{n}-day sum of the
#' absolute value of the \code{n}-day momentum, minus (3) \code{n}-day sum of
#' the absolute value of the \code{n}-day momentum.
#' I.e. \eqn{TDI = (1) - [ (2) - (3) ]}
#' The direction indicator is the sum of the \code{n}-day momentum over the last
#' \code{n} days.
#' See URL in references section for further details.
#' @param OHLCV Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n Number of periods to use. Default is 20.
#' @param multiple Multiple used to calculate (2). Default is 2.
#' @param allow_middle_na Logical. If \code{TRUE}, allows NA values in the middle. Default is FALSE.
#' @param append A logical value. If \code{TRUE}, the calculated Trend Detection Index
#' and Direction Indicator values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Trend Detection Index and Direction Indicator values will be returned. Defaults to \code{FALSE}.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a matrix (if \code{try.xts} fails) containing the columns:
#'  \describe{
#'   \item{ tdi }{ The Trend Detection Index. }
#'   \item{ di }{ The Direction Indicator. }
#'  }
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Trend Detection Index and Direction Indicator values appended, maintaining the integrity of the time - series
#' alignment.
#' @note Positive/negative TDI values signal a trend/consolidation. A positive/
#' negative direction indicator signals a up/down trend. I.e. buy if the TDI
#' and the direction indicator are positive, and sell if the TDI is positive
#' while the direction indicator is negative.
#' @author DengYishuo
#' @seealso See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{ADX}},
#' \code{\link{VHF}}, \code{\link{GMMA}} for other indicators that measure trend
#' direction/strength.
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://www.linnsoft.com/techind/trend - detection - index - tdi}\cr
#' @keywords ts
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' tdi_result1 <- add_TDI(TSLA)
#'
#' # Modifying n and without appending
#' tdi_result2 <- add_TDI(TSLA, n = 25)
#'
#' # Using default parameters and appending
#' tdi_result3 <- add_TDI(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' tdi_result4 <- add_TDI(TSLA, n = 25, append = TRUE)
#' }
#' @export
add_TDI <- function(OHLCV, n = 20, multiple = 2, allow_middle_na = FALSE, append = FALSE) {
  # Assume we use Close price for calculation, can be adjusted
  price <- OHLCV[, "Close"]

  if (is.null(price)) stop("price cannot be NULL")
  if (n <= 0 || multiple <= 0) stop("n and multiple must be positive")
  if (!is.logical(allow_middle_na) || length(allow_middle_na) != 1) {
    stop("allow_middle_na must be a logical value")
  }

  price <- try.xts(price, error = as.matrix)
  price_len <- nrow(price)

  if (n >= price_len) stop("n must be smaller than data length")

  if (anyNA(price)) {
    first_non_na <- min(which(!is.na(price[, 1])))

    if (!allow_middle_na && anyNA(price[first_non_na:nrow(price), ])) {
      stop("TDI requires all NA values to be leading when allow_middle_na is FALSE")
    }

    if (first_non_na > 1) {
      price <- price[first_non_na:price_len, ]
      price_len <- nrow(price)

      if (price_len < n) {
        stop("insufficient non - NA data after removing leading NA")
      }
    }
  }

  mom <- momentum(price, n, na.pad = TRUE)
  mom[is.na(mom)] <- 0

  di <- runSum(mom, n)
  abs.di <- abs(di)

  max_run_sum <- price_len - n + 1
  run_sum_2n <- min(n * multiple, max_run_sum)
  run_sum_2n <- max(run_sum_2n, 1)

  run_sum_1n <- n
  run_sum_1n <- max(run_sum_1n, 1)

  abs.mom.2n <- runSum(abs(mom), run_sum_2n)
  abs.mom.1n <- runSum(abs(mom), run_sum_1n)

  tdi <- abs.di - (abs.mom.2n - abs.mom.1n)

  result <- cbind(tdi, di)
  colnames(result) <- c("tdi", "di")
  result <- reclass(result, price)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, result)
    return(combined_result)
  } else {
    return(result)
  }
}
