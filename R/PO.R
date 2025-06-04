#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2007-2013  Deng Yishuo
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

#' @title Price Oscillator (PO) Calculation
#' 价格振荡指标(PO)计算
#' @description
#' Calculates the Price Oscillator (PO), a momentum indicator that measures the difference
#' between two moving averages of different periods. It helps identify trends and potential
#' reversal points in financial markets.
#' 计算价格振荡指标(PO)，一种衡量两个不同周期移动平均线差异的动量指标。
#' 它有助于识别金融市场中的趋势和潜在反转点。
#'
#' @param price A price series, which can be a vector, time series, or xts object.
#'              价格序列，可以是向量、时间序列或xts对象。
#' @param n_short The period for the short-term moving average, default is 12.
#'                短期移动平均的周期，默认值12。
#' @param n_long The period for the long-term moving average, default is 26.
#'               长期移动平均的周期，默认值26。
#' @param type The type of return value: "difference" for the absolute difference,
#'             "percent" for the percentage difference. Default is "difference".
#'             返回值类型："difference"为绝对差值形式，"percent"为百分比形式，默认"difference"。
#' @param ma_type The type of moving average, supports "SMA" (Simple Moving Average)
#'                and "EMA" (Exponential Moving Average). Default is "SMA".
#'                移动平均类型，支持"SMA"(简单移动平均)、"EMA"(指数移动平均)，默认"SMA"。
#'
#' @return A PO indicator series of the same type as the input price series.
#'         返回与输入价格序列相同类型的PO指标序列。
#' @importFrom utils head
#' @importFrom xts xts
#' @export
#'
#' @details
#' The Price Oscillator (PO) measures momentum by calculating the difference between
#' a short-term and a long-term moving average of prices. A positive PO value indicates
#' upward momentum, while a negative value suggests downward momentum.
#' 价格振荡指标(PO)通过计算价格的短期和长期移动平均线之间的差异来衡量动量。
#' 正值表示上升动量，负值表示下降动量。
#'
#' @references
#' Murphy, J. (1999). Technical Analysis of the Financial Markets. New York Institute of Finance.
#'
#' @seealso
#' \code{\link{SMA}}, \code{\link{EMA}} for moving average calculations.
#' 查看 \code{\link{SMA}}, \code{\link{EMA}} 用于移动平均计算的函数。
#'
#' @examples
#' # Demonstrate PO calculation with random data
#' # 使用随机数据演示PO计算
#' set.seed(123)
#' price_data <- xts::xts(rnorm(100, 100, 5), order.by = Sys.Date() - 100:1)
#'
#' # Calculate PO with default parameters (difference form)
#' # 计算默认参数的PO指标(差值形式)
#' po_diff <- PO(price_data)
#'
#' # Calculate PO in percentage form
#' # 计算百分比形式的PO指标
#' po_percent <- PO(price_data, type = "percent")
#'
#' # Calculate PO using EMA
#' # 使用EMA计算PO指标
#' po_ema <- PO(price_data, ma_type = "EMA")
PO <- function(price, n_short = 12, n_long = 26,
               type = c("difference", "percent"),
               ma_type = c("SMA", "EMA")) {
  # Validate input price series
  # 验证输入价格序列
  if (missing(price)) {
    stop("Price series must be provided as input")
  }

  # Check if price series is a vector, time series, or xts object
  # 检查价格序列是否为向量、时间序列或xts对象
  if (!is.vector(price) && !is.ts(price) && !inherits(price, "xts")) {
    stop("Price series must be a vector, time series, or xts object")
  }

  # Validate moving average period parameters
  # 验证移动平均周期参数
  if (!is.numeric(n_short) || !is.numeric(n_long) ||
    n_short <= 0 || n_long <= 0 ||
    n_short >= n_long) {
    stop("Moving average periods must be positive integers, and n_short must be smaller than n_long")
  }

  # Validate return type parameter
  # 验证返回值类型参数
  type <- match.arg(type)

  # Validate moving average type parameter
  # 验证移动平均类型参数
  ma_type <- match.arg(ma_type)

  # Calculate moving averages based on specified type
  # 根据指定类型计算移动平均线
  if (ma_type == "SMA") {
    # Calculate simple moving averages
    # 计算简单移动平均线
    sma_short <- SMA(price, n = n_short)
    sma_long <- SMA(price, n = n_long)
  } else {
    # Calculate exponential moving averages
    # 计算指数移动平均线
    sma_short <- EMA(price, n = n_short)
    sma_long <- EMA(price, n = n_long)
  }

  # Calculate PO indicator based on specified type
  # 根据指定类型计算PO指标
  if (type == "difference") {
    # Difference form: short MA - long MA
    # 差值形式: 短期均线 - 长期均线
    po <- sma_short - sma_long
  } else {
    # Percentage form: (short MA - long MA) / long MA * 100%
    # 百分比形式: (短期均线 - 长期均线) / 长期均线 * 100%
    po <- ((sma_short - sma_long) / sma_long) * 100
  }
  colnames(po) <- "PO"
  # Return the calculated result
  # 返回计算结果
  return(po)
}
