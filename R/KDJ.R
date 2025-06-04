#' Calculate Stochastic Oscillator (KDJ) Indicator
#'
#' This function computes the KDJ indicator based on an OHLC (Open, High, Low, Close) xts object,
#' automatically extracting required columns ('high', 'low', 'close') from the input (case-insensitive).
#' The KDJ indicator helps identify overbought or oversold conditions, returning K, D, and J lines.
#'
#' @param ohlc xts object with OHLC data, must include 'high', 'low', and 'close' columns (case-insensitive).
#' @param n RSV calculation period, default is 9.
#' @param m1 Smoothing factor for K line, default is 3.
#' @param m2 Smoothing factor for D line, default is 3.
#' @param fill_na_method Method to fill NA values: "none", "initial", "interpolate".
#'
#' @return An xts object with three columns: 'K', 'D', and 'J'.
#' @importFrom xts xts is.xts
#' @importFrom zoo rollapply na.approx index
#' @export
#' @examples
#' data(TSLA)
#' tsla_kdj <- KDJ(TSLA, n = 9, m1 = 3, m2 = 3, fill.na.method = "interpolate")
#'
KDJ <- function(ohlc, n = 9, m1 = 3, m2 = 3, fill_na_method = "none") {
  if (!is.xts(ohlc)) stop("Input 'ohlc' must be an xts object.")

  # 标准化列名
  input_cols <- tolower(colnames(ohlc))
  required_cols <- c("high", "low", "close")

  # 检查必要列是否存在
  missing_cols <- required_cols[!required_cols %in% input_cols]
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # 提取必要列
  high_idx <- which(input_cols == "high")
  low_idx <- which(input_cols == "low")
  close_idx <- which(input_cols == "close")

  high <- ohlc[, high_idx]
  low <- ohlc[, low_idx]
  close <- ohlc[, close_idx]

  time_index <- index(ohlc)
  len <- length(close)

  # 计算RSV所需的高低价
  highest_high <- rollapply(high, n, max, align = "right", fill = NA)
  lowest_low <- rollapply(low, n, min, align = "right", fill = NA)

  # 计算RSV
  rsv <- xts(rep(NA, len), order.by = time_index)
  valid_idx <- which(!is.na(highest_high) & !is.na(lowest_low) & (highest_high != lowest_low))
  zero_div_idx <- which(!is.na(highest_high) & !is.na(lowest_low) & (highest_high == lowest_low))

  rsv[valid_idx] <- 100 * (close[valid_idx] - lowest_low[valid_idx]) /
    (highest_high[valid_idx] - lowest_low[valid_idx])
  rsv[zero_div_idx] <- 50 # 处理除数为零的情况

  # 初始化K、D、J
  k <- xts(rep(NA, len), order.by = time_index)
  d <- xts(rep(NA, len), order.by = time_index)
  j <- xts(rep(NA, len), order.by = time_index)

  # 找到第一个有效RSV的位置
  first_valid <- which(!is.na(rsv))[1]
  if (is.na(first_valid)) {
    warning("No valid RSV values. Returning all NA.")
    return(xts(matrix(NA, nrow = len, ncol = 3),
      order.by = time_index,
      dimnames = list(NULL, c("K", "D", "J"))
    ))
  }

  # 设置初始值
  k[first_valid] <- 50
  d[first_valid] <- 50

  # 计算后续K、D值
  for (i in (first_valid + 1):len) {
    prev_k <- ifelse(is.na(k[i - 1]), 50, k[i - 1])
    prev_d <- ifelse(is.na(d[i - 1]), 50, d[i - 1])

    k[i] <- (1 / m1) * rsv[i] + (1 - 1 / m1) * prev_k
    d[i] <- (1 / m2) * k[i] + (1 - 1 / m2) * prev_d
  }

  j <- 3 * k - 2 * d

  # 处理缺失值
  if (fill_na_method == "initial" && first_valid > 1) {
    initial_val <- 50
    k[1:(first_valid - 1)] <- initial_val
    d[1:(first_valid - 1)] <- initial_val
    j[1:(first_valid - 1)] <- 3 * initial_val - 2 * initial_val
  } else if (fill_na_method == "interpolate") {
    k <- na.approx(k, na.rm = FALSE)
    d <- na.approx(d, na.rm = FALSE)
    j <- 3 * k - 2 * d
  }

  result <- cbind(K = k, D = d, J = j)
  return(result)
}
