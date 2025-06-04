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

#' @title Relative Vigor Index (RVI) Calculation
#' 相对活力指数(RVI)计算
#' @description
#' Computes the Relative Vigor Index (RVI), a momentum oscillator that measures the
#' strength of a trend by comparing closing prices to trading ranges. The RVI is
#' considered bullish when above its signal line and bearish when below.
#' 计算相对活力指数(RVI)，这是一种动量振荡指标，通过比较收盘价与交易区间来衡量趋势强度。
#' 当RVI高于其信号线时被视为看涨，低于时被视为看跌。
#'
#' @param price A numeric vector or xts object containing price data.
#'              包含价格数据的数值向量或xts对象。
#' @param n The period for calculating standard deviation, default is 14.
#'          计算标准差的周期，默认值14。
#' @param ema.n The period for exponential moving average (EMA) smoothing, default is 3.
#'              指数移动平均(EMA)平滑的周期，默认值3。
#' @param keepNA Logical indicating whether to keep NA values in the result. Default is TRUE.
#'               逻辑值，指示是否在结果中保留NA值。默认值TRUE。
#'
#' @return An object of the same type as the input (vector or xts) containing RVI values.
#'         返回与输入相同类型的对象(向量或xts)，包含RVI值。
#' @export
#'
#' @details
#' The RVI is calculated by:
#' 1. Separating price changes into upward and downward movements
#' 2. Calculating the standard deviation of these movements over a specified period
#' 3. Applying exponential smoothing to the standard deviations
#' 4. Computing the ratio of smoothed upward movements to total movements
#'
#' RVI的计算方法为：
#' 1. 将价格变动分为上涨和下跌运动
#' 2. 计算指定周期内这些运动的标准差
#' 3. 对标准差应用指数平滑
#' 4. 计算平滑后的上涨运动与总运动的比率
#'
#' @references
#' 1. Wilder, J. Welles (1978). New Concepts in Technical Trading Systems.
#' 2. Carver, Constance M. (1992). The New Technical Trader.
#'
#' @seealso
#' \code{\link{EMA}} for exponential moving average calculation.
#' \code{\link{runSD}} for rolling standard deviation calculation.
#'
#' 查看 \code{\link{EMA}} 用于指数移动平均计算。
#' 查看 \code{\link{runSD}} 用于滚动标准差计算。
#'
#' @examples
#' # Calculate RVI with default parameters
#' # 使用默认参数计算RVI
#' price_data <- rnorm(100, 100, 5)
#' rvi_result <- RVI(price_data)
#'
#' # Calculate RVI with custom parameters and plot
#' # 使用自定义参数计算RVI并绘图
#' library(xts)
#' price_xts <- xts(price_data, order.by = Sys.Date() - 99:0)
#' rvi_custom <- RVI(price_xts, n = 20, ema.n = 5)
#' plot(rvi_custom, main = "Custom RVI Parameters")
RVI <- function(price, n = 14, ema.n = 3, keepNA = TRUE) {
  # Input validation
  # 输入验证
  if (!is.xts(price) && !is.numeric(price)) {
    stop("price must be an xts object or a numeric vector")
  }

  if (!is.numeric(n) || n <= 0 || !all.equal(n, as.integer(n))) {
    stop("n must be a positive integer")
  }
  n <- as.integer(n)

  if (!is.numeric(ema.n) || ema.n <= 0) {
    stop("ema.n must be a positive numeric value")
  }

  if (!is.logical(keepNA)) {
    stop("keepNA must be a logical value")
  }

  # Save original timestamp and length
  # 保存原始时间戳和长度
  original_index <- if (is.xts(price)) index(price) else NULL
  original_length <- length(price)

  # Calculate price changes and align timestamps
  # 计算价格变动并对齐时间戳
  price_change <- diff(price)
  price_change <- align_with_index(price_change, original_index, NA)

  # Separate upward and downward changes and align timestamps
  # 分离上涨和下跌变动并对齐时间戳
  up_change <- ifelse(price_change > 0, price_change, 0)
  down_change <- ifelse(price_change < 0, abs(price_change), 0)

  # Calculate n-period standard deviation and align timestamps
  # 计算n周期标准差并对齐时间戳
  up_sd <- runSD(up_change, n = n)
  down_sd <- runSD(down_change, n = n)
  up_sd <- align_with_index(up_sd, original_index, NA)
  down_sd <- align_with_index(down_sd, original_index, NA)

  # Apply EMA smoothing to standard deviations and align timestamps
  # 对标准差进行EMA平滑并对齐时间戳
  up_ema <- EMA(up_sd, n = ema.n)
  down_ema <- EMA(down_sd, n = ema.n)
  up_ema <- align_with_index(up_ema, original_index, NA)
  down_ema <- align_with_index(down_ema, original_index, NA)

  # Calculate RVI values, handle division by zero
  # 计算RVI值，处理除零情况
  rvi <- 100 * (up_ema / (up_ema + down_ema))

  # Final alignment
  # 最终对齐
  if (!is.null(original_index)) {
    rvi <- align_with_index(rvi, original_index, NA)
    colnames(rvi) <- "RVI"
  } else {
    if (length(rvi) < original_length) {
      rvi <- c(rep(NA, original_length - length(rvi)), rvi)
    }
    names(rvi) <- "RVI"
  }

  # Decide whether to keep NA values based on keepNA parameter
  # 根据keepNA参数决定是否保留NA值
  if (!keepNA) {
    if (is.xts(rvi)) {
      # NA filtering that preserves index for xts objects
      # 对xts对象保留索引的NA过滤
      rvi <- rvi[!is.na(rvi)]
    } else {
      rvi <- na.omit(rvi)
    }
  }

  return(rvi)
}

#' Align an object with a target index
#' 使对象与目标索引对齐
#'
#' Ensures that the input object is aligned with the specified target index,
#' filling missing values with NA if necessary.
#' 确保输入对象与指定的目标索引对齐，必要时用NA填充缺失值。
#'
#' @param x The input object (numeric vector or xts).
#'          输入对象(数值向量或xts)。
#' @param target_index The target index to align with.
#'                     要对齐的目标索引。
#' @param fill_value The value to use for filling missing positions, default is NA.
#'                   用于填充缺失位置的值，默认值NA。
#'
#' @return An xts object aligned with the target index.
#'         与目标索引对齐的xts对象。
#' @keywords internal
align_with_index <- function(x, target_index, fill_value = NA) {
  if (is.null(target_index)) {
    return(x)
  }

  # If x is already xts and length matches, return directly
  # 如果x已经是xts且长度匹配，直接返回
  if (is.xts(x) && length(index(x)) == length(target_index)) {
    return(x)
  }

  # If x is not xts, convert to xts
  # 如果x不是xts，转换为xts
  if (!is.xts(x)) {
    x_xts <- xts(rep(fill_value, length(target_index)), order.by = target_index)
    if (length(x) > 0) {
      x_xts[(length(target_index) - length(x) + 1):length(target_index)] <- x
    }
    return(x_xts)
  }

  # If x is xts but length doesn't match, extend or truncate
  # 如果x是xts但长度不匹配，扩展或截断
  current_length <- length(index(x))
  target_length <- length(target_index)

  if (current_length == target_length) {
    # Same length but indices might differ, reset index
    # 长度相同但索引可能不同，重新设置索引
    index(x) <- target_index
    return(x)
  } else if (current_length < target_length) {
    # Extend and fill with NA
    # 扩展并填充NA
    new_x <- xts(rep(fill_value, target_length), order.by = target_index)
    new_x[(target_length - current_length + 1):target_length] <- coredata(x)
    return(new_x)
  } else {
    # Truncate
    # 截断
    return(head(x, target_length))
  }
}
