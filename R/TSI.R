#' True Strength Index (TSI) Calculation
#' 真实强度指数(TSI)计算
#' @param price A numeric vector or xts object containing price data.
#'              包含价格数据的数值向量或xts对象。
#' @param r The period for the first exponential moving average (fast EMA), default is 13.
#'          第一个指数移动平均(快速EMA)的周期，默认值13。
#' @param s The period for the second exponential moving average (slow EMA), default is 25.
#'          第二个指数移动平均(慢速EMA)的周期，默认值25。
#' @param signal_period The period for the signal line (EMA of TSI), default is 9.
#'                      信号线(TSI的EMA)的周期，默认值9。
#' @param ... Additional arguments passed to the EMA function (e.g., wilder).
#'           传递给EMA函数的其他参数(例如wilder)。
#' @return An object of the same type as the input (vector or xts) containing two columns:
#'         "tsi" (True Strength Index values) and "signal" (signal line values).
#'         返回与输入相同类型的对象(向量或xts)，包含两列：
#'         "tsi"(真实强度指数值)和"signal"(信号线值)。
#' @export
TSI <- function(price, r = 13, s = 25, signal_period = 9, ...) {
  # 输入验证
  if (!is.numeric(price) && !inherits(price, "xts")) {
    stop("Price must be a numeric vector or xts object")
  }

  price_data <- as.numeric(price)
  original_index <- if (inherits(price, "xts")) index(price) else NULL

  # 处理价格变动（避免手动添加NA，直接保留diff后的结果）
  price_change <- diff(price_data)

  # 计算EMA时保留足够的NA，确保最终结果与原始数据对齐
  ema1 <- EMA(price_change, n = r, ...)
  ema2 <- EMA(ema1, n = s, ...)
  abs_ema1 <- EMA(abs(price_change), n = r, ...)
  abs_ema2 <- EMA(abs_ema1, n = s, ...)

  # 计算TSI（此时结果长度为 length(price_data) - r - s - signal_period + 3，需对齐）
  tsi <- (ema2 / abs_ema2) * 100
  signal <- EMA(tsi, n = signal_period, ...)

  # 组合结果并对齐索引
  result <- cbind(tsi, signal)
  colnames(result) <- c("tsi", "signal")

  # 处理xts对象的索引对齐
  if (inherits(price, "xts")) {
    # 计算结果的起始位置：原始价格索引从第 (r + s) 个位置开始（因EMA计算需要至少r+s个数据）
    valid_indices <- seq(from = r + s, to = length(price_data), by = 1)
    if (length(valid_indices) < length(result)) {
      valid_indices <- tail(valid_indices, length(result))
    }
    # 确保order.by的长度与result行数一致
    if (length(valid_indices) != nrow(result)) {
      valid_indices <- valid_indices[1:nrow(result)]
    }
    result <- xts(result, order.by = original_index[valid_indices])
  }

  return(result)
}
