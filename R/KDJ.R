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
#' 计算KDJ指标（随机指标）｜Calculate Stochastic Oscillator (KDJ) Indicator
#'
#' 该函数基于给定的最高价、最低价和收盘价序列计算KDJ指标。｜This function calculates the KDJ indicator based on the given high, low, and close price series.
#' KDJ指标是一种技术分析工具，用于判断价格是否超买或超卖，｜The KDJ indicator is a technical analysis tool used to identify overbought or oversold conditions in the market.
#' 由三条曲线组成：K线、D线和J线。｜It consists of three lines: K line, D line, and J line.
#'
#' @param high 最高价序列，通常是OHLCV数据中的最高价列｜High price series, typically the high column from OHLCV data
#' @param low 最低价序列，通常是OHLCV数据中的最低价列｜Low price series, typically the low column from OHLCV data
#' @param close 收盘价序列，通常是OHLCV数据中的收盘价列｜Close price series, typically the close column from OHLCV data
#' @param n RSV计算周期，默认为9｜RSV calculation period, default is 9
#' @param m1 K值平滑因子，默认为3｜Smoothing factor for K line, default is 3
#' @param m2 D值平滑因子，默认为3｜Smoothing factor for D line, default is 3
#' @param ratio 初始值填充比例，用于处理前n-1个缺失值，默认为0.5｜Initial value filling ratio for handling missing values in the first n-1 periods, default is 0.5
#'
#' @return 返回一个数据框，包含三列：K（K线值）、D（D线值）和J（J线值）｜Returns a data frame with three columns: K (K line values), D (D line values), and J (J line values)
#' @export
#'
#' @examples
#' # 使用quantmod包获取股票数据并计算KDJ指标｜Get stock data and calculate KDJ using quantmod
#' library(quantmod)
#' getSymbols("AAPL")
#' kdj <- KDJ(high = Hi(AAPL), low = Lo(AAPL), close = Cl(AAPL))
#' head(kdj)
#'
#' @importFrom zoo coredata
#' @importFrom zoo na.locf
#'
KDJ <- function(high, low, close, n = 9, m1 = 3, m2 = 3, ratio = 0.5) {
  # 转换为数值向量｜Convert to numeric vectors
  high <- as.numeric(high)
  low <- as.numeric(low)
  close <- as.numeric(close)

  # 检查输入长度是否一致｜Check if inputs have the same length
  if (length(high) != length(low) || length(high) != length(close)) {
    stop("输入的high、low和close序列长度必须相同｜Inputs high, low, and close must have the same length")
  }

  # 计算RSV (Raw Stochastic Value)
  len <- length(close)
  rsv <- numeric(len)
  rsv[] <- NA

  # 高效计算n日内最高价和最低价｜Efficiently calculate n-period high and low
  highest_high <- runMax(high, n)
  lowest_low <- runMin(low, n)

  # 计算RSV值｜Calculate RSV values
  for (i in n:len) {
    if (highest_high[i] == lowest_low[i]) {
      rsv[i] <- 50 # 防止除零错误，当最高价等于最低价时，RSV设为50｜Prevent division by zero
    } else {
      rsv[i] <- 100 * (close[i] - lowest_low[i]) / (highest_high[i] - lowest_low[i])
    }
  }

  # 初始化K、D、J向量｜Initialize K, D, J vectors
  k <- numeric(len)
  d <- numeric(len)
  j <- numeric(len)
  k[] <- NA
  d[] <- NA
  j[] <- NA

  # 设置初始值｜Set initial values
  first_valid <- which(!is.na(rsv))[1]
  if (!is.na(first_valid)) {
    k[first_valid] <- 50 # 初始K值｜Initial K value
    d[first_valid] <- 50 # 初始D值｜Initial D value

    # 计算后续K、D、J值｜Calculate subsequent K, D, J values
    for (i in (first_valid + 1):len) {
      k[i] <- (1 / m1) * rsv[i] + (1 - 1 / m1) * k[i - 1]
      d[i] <- (1 / m2) * k[i] + (1 - 1 / m2) * d[i - 1]
      j[i] <- 3 * k[i] - 2 * d[i]
    }
  }

  # 创建结果数据框｜Create result data frame
  result <- data.frame(K = k, D = d, J = j)

  # 处理前n-1个缺失值（可选）｜Handle missing values in the first n-1 periods (optional)
  if (ratio > 0 && ratio <= 1 && first_valid > 1) {
    # 使用初始值和50的加权平均填充｜Fill with weighted average of initial value and 50
    initial_value <- ratio * rsv[first_valid] + (1 - ratio) * 50
    result$K[1:(first_valid - 1)] <- initial_value
    result$D[1:(first_valid - 1)] <- initial_value
    result$J[1:(first_valid - 1)] <- 3 * initial_value - 2 * initial_value
  }

  return(result)
}
