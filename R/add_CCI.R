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
#' @title Calculate Commodity Channel Index
#' @description
#' The Commodity Channel Index (CCI) attempts to identify starting and ending
#' trends.
#' @details
#' CCI relates the current price and the average of price over \code{n} periods.
#' The CCI usually falls in a channel of - 100 to 100. A basic CCI trading system
#' is: Buy (sell) if CCI rises above 100 (falls below - 100) and sell (buy) when
#' it falls below 100 (rises above - 100).
#' CCI is usually calculated using the typical price, but if a univariate series
#' (e.g. Close, Weighted Close, Median Price, etc.) is provided, it will be used
#' instead.
#' @param OHLCV Object that is coercible to xts or matrix and contains
#' Open - High - Low - Close - Volume prices.
#' @param n Number of periods for moving average. Defaults to 20.
#' @param maType A function or a string naming the function to be called.
#' @param c Constant to apply to the mean deviation. Defaults to 0.015.
#' @param append A logical value. If \code{TRUE}, the calculated CCI values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' CCI values will be returned. Defaults to \code{FALSE}.
#' @param ... Other arguments to be passed to the \code{maType} function.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the CCI values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated CCI values appended, maintaining the integrity of the time - series
#' alignment.
#' @note If \code{OHLCV} contains High - Low - Close prices, then typical price will be
#' calculated.  If \code{OHLCV} is a univariate series, then those values will be used
#' instead of the typical price.
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.  See \code{\link{aroon}},
#' \code{\link{ADX}}, \code{\link{TDI}}, \code{\link{VHF}}, \code{\link{GMMA}}
#' for other indicators that measure trend direction/strength.
#' @references The following site(s) were used to code/document this
#' indicator:\cr \url{https://www.fmlabs.com/reference/CCI.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=42}\cr
#' \url{https://www.linnsoft.com/techind/cci - commodity - channel - index}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:commodity_channel_index_cci}\cr
#' @keywords ts
#' @importFrom xts try.xts xcoredata
#' @importFrom zoo coredata
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' cci_result1 <- add_CCI(TSLA)
#'
#' # Modifying n and without appending
#' cci_result2 <- add_CCI(TSLA, n = 30)
#'
#' # Using default parameters and appending
#' cci_result3 <- add_CCI(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' cci_result4 <- add_CCI(TSLA, n = 30, append = TRUE)
#' }
#' @export
add_CCI <- function(OHLCV, n = 20, maType, c = 0.015, append = FALSE, ...) {
  # Commodity Channel Index
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
  # Default MA
  if (missing(maType)) {
    maType <- "SMA"
  }

  mavg <- do.call(maType, c(list(hlc), ma_args))
  mean_dev <- runMAD(hlc, n, center = mavg, stat = "mean")

  cci <- (hlc - mavg) / (c * mean_dev)

  if (is.xts(cci)) {
    colnames(cci) <- "cci"
  }

  cci <- reclass(cci, hlc)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, cci)
    return(combined_result)
  } else {
    return(cci)
  }
}
