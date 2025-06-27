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
#' @title Calculate Chaikin Volatility
#' @description
#' Chaikin Volatility is an indicator that measures the rate of change of a security's trading range.
#' Developed by Marc Chaikin, it defines volatility as an increase in the difference between the high and low prices.
#' This metric is useful for traders to identify potential trend reversals. A rapid increase in Chaikin Volatility
#' might suggest that the market is becoming more volatile, potentially indicating an approaching bottom.
#' Conversely, a slow decrease could imply an approaching top.
#'
#' @param OHLCV Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
#' The function will extract the High and Low prices from this object for Chaikin Volatility calculation.
#' @param n Number of periods for moving average. This parameter determines the window size for calculating
#' the moving average of the high - low price differences. A larger \code{n} will result in a smoother moving average,
#' which can help filter out short - term price fluctuations. However, it may also make the indicator less responsive
#' to recent price changes. The valid range for \code{n} is a positive integer.
#' @param maType A function or a string naming the function to be called. This specifies the type of moving average to use.
#' Common options include "EMA" (Exponential Moving Average), "SMA" (Simple Moving Average), etc. If a string is provided,
#' it should match the name of a valid moving average function.
#' @param append A logical value. If \code{TRUE}, the calculated Chaikin Volatility values will be appended to the \code{OHLCV}
#' input data, ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated Chaikin Volatility
#' values will be returned. Defaults to \code{FALSE}.
#' @param... Other arguments to be passed to the \code{maType} function. These can be used to customize the moving average
#' calculation, such as the smoothing factor for an EMA.
#' @return
#' If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
#' containing the Chaikin Volatility values. The returned object will have the same time - series index as the input
#' \code{OHLCV} data (if applicable), with the calculated Chaikin Volatility values in a single column.
#' If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated Chaikin Volatility
#' values appended as an additional column, maintaining the integrity of the time - series alignment.
#' @note
#' In addition to the general interpretations of Chaikin Volatility for trend reversals, it's important to note that
#' this indicator should be used in conjunction with other technical analysis tools. Market conditions can be complex,
#' and relying solely on Chaikin Volatility may lead to inaccurate trading decisions. Also, the choice of moving average
#' type (\code{maType}) can significantly impact the results. For example, an EMA gives more weight to recent data,
#' which may be more sensitive to short - term price changes compared to an SMA.
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note Warning section.
#' See \code{\link{TR}} for another volatility measure.
#' @references The following site(s) were used to code/document this indicator:\cr
#' \url{https://www.fmlabs.com/reference/ChaikinVolatility.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=120}\cr
#' @export
#' @keywords ts
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' volatility1 <- add_chaikinVolatility(TSLA)
#'
#' # Using default parameters and appending
#' volatility2 <- add_chaikinVolatility(TSLA, append = TRUE)
#'
#' # Changing n and without appending
#' volatility3 <- add_chaikinVolatility(TSLA, n = 15)
#'
#' # Changing n and appending
#' volatility4 <- add_chaikinVolatility(TSLA, n = 15, append = TRUE)
#' }
add_chaikinVolatility <- function(OHLCV, n = 10, maType, append = FALSE, ...) {
  # 检查OHLCV是否包含High和Low列
  required_cols <- c("High", "Low")
  if (!all(required_cols %in% colnames(OHLCV))) {
    stop("OHLCV must contain 'High' and 'Low' columns")
  }

  # 从OHLCV中提取High和Low价格数据
  hl <- OHLCV[, required_cols]
  hl <- try.xts(hl, error = as.matrix)

  ma_args <- list(n = n, ...)

  # 检查maType参数
  if (is.character(maType)) {
    maType <- match.fun(maType)
  } else if (!is.function(maType)) {
    stop("maType must be a function or a string naming a function")
  }

  # 计算高低价格差的移动平均
  mavg <- do.call(maType, c(list(hl[, 1] - hl[, 2]), ma_args))

  # 计算移动平均的离散型变化率
  volatility <- ROC(mavg, n, type = "discrete")

  volatility <- reclass(volatility, hl)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, volatility)
    return(combined_result)
  } else {
    return(volatility)
  }
}
