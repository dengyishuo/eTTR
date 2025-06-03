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

#' Sliding Window Linear Regression
#' 滑动窗口线性回归
#'
#' Performs linear regression over a sliding window on a time series,
#' providing coefficients and statistical metrics for each window.
#' 在时间序列上执行滑动窗口线性回归，为每个窗口提供回归系数和统计指标。
#'
#' @param x A time series (vector or xts object).
#'          输入的时间序列（向量或xts对象）。
#' @param n The size of the sliding window (number of periods for regression). Default is 14.
#'          滑动窗口大小（回归计算的周期数），默认值14。
#' @param include.intercept Logical indicating whether to include an intercept term. Default is TRUE.
#'                          是否包含截距项，逻辑值，默认TRUE。
#' @param output The type of output: "all" returns all regression results, "slope" returns only the slope.
#'               返回结果类型，"all"返回全部回归结果，"slope"仅返回斜率。
#'
#' @return If output is "slope", returns a vector or xts object containing slope values.
#'         If output is "all", returns a list of class "runRegression" containing coefficients,
#'         R-squared, adjusted R-squared, F-statistic, and p-value.
#'         若output为"slope"，返回包含斜率值的向量或xts对象；
#'         若output为"all"，返回类"runRegression"的列表，包含系数、R²、调整R²、F统计量和p值。
#' @importFrom xts xts
#' @export
#'
#' @details
#' This function computes linear regression over a sliding window of specified size.
#' It is particularly useful for analyzing trends and momentum in financial time series.
#' 该函数在指定大小的滑动窗口上计算线性回归，特别适用于分析金融时间序列的趋势和动量。
#'
#' @examples
#' # Generate sample data
#' # 生成示例数据
#' set.seed(123)
#' x <- xts::xts(rnorm(100), order.by = Sys.Date() - 99:0)
#'
#' # Calculate slope-only results
#' # 仅计算斜率结果
#' slopes <- runRegression(x, n = 10, output = "slope")
#'
#' # Calculate full regression results
#' # 计算完整回归结果
#' reg_results <- runRegression(x, n = 10, output = "all")
runRegression <- function(x, n = 14, include.intercept = TRUE, output = "all") {
  # Input validation
  # 参数校验
  if (!is.numeric(x)) {
    stop("Input x must be a numeric vector or xts object")
  }

  if (length(x) < n) {
    stop("Input length must be greater than window size n")
  }

  if (n <= 1) {
    stop("Window size n must be greater than 1")
  }

  if (!is.logical(include.intercept)) {
    stop("include.intercept must be a logical value (TRUE or FALSE)")
  }

  if (!output %in% c("all", "slope")) {
    stop("output parameter must be 'all' or 'slope'")
  }

  # Initialize result matrix
  # 初始化结果矩阵
  result_length <- length(x) - n + 1

  # Set coefficient names
  # 设置系数列名
  coef_names <- if (include.intercept) {
    c("intercept", paste0("slope_", n, "d"))
  } else {
    paste0("slope_", n, "d")
  }

  coefficients <- matrix(NA, nrow = result_length, ncol = length(coef_names))
  colnames(coefficients) <- coef_names

  # Create time index (if input is xts)
  # 创建时间索引（如果输入是xts对象）
  if (inherits(x, "xts")) {
    result_index <- index(x)[n:length(x)]
  } else {
    result_index <- n:length(x)
  }

  # Create x variable (time index)
  # 创建x变量（时间索引）
  x_idx <- 1:n

  # Preallocate result vectors
  # 预分配结果向量
  r_squared <- numeric(result_length)
  adj_r_squared <- numeric(result_length)
  f_statistic <- numeric(result_length)
  p_value <- numeric(result_length)

  # Efficiently compute sliding window regression
  # 高效计算滑动窗口回归
  for (i in 1:result_length) {
    # Get data for current window
    # 获取当前窗口的数据
    window_data <- x[i:(i + n - 1)]

    # Build model formula
    # 构建模型公式
    if (include.intercept) {
      model <- lm(window_data ~ x_idx)
    } else {
      model <- lm(window_data ~ x_idx - 1)
    }

    # Extract regression coefficients
    # 提取回归系数
    coefficients[i, ] <- coef(model)

    # Extract goodness of fit
    # 提取拟合优度
    summary_model <- summary(model)
    r_squared[i] <- summary_model$r.squared
    adj_r_squared[i] <- summary_model$adj.r.squared

    # Extract F-statistic and p-value
    # 提取F统计量和p值
    f_statistic[i] <- summary_model$fstatistic[1]
    p_value[i] <- pf(f_statistic[i], summary_model$fstatistic[2],
      summary_model$fstatistic[3],
      lower.tail = FALSE
    )
  }

  # Determine return content based on output parameter
  # 根据output参数决定返回内容
  if (output == "slope") {
    # Return only slope
    # 仅返回斜率
    slope_col <- if (include.intercept) 2 else 1
    result <- coefficients[, slope_col, drop = FALSE]

    if (inherits(x, "xts")) {
      result <- xts(result, order.by = result_index)
      colnames(result) <- coef_names[slope_col]
    }

    return(result)
  } else {
    # Return all results
    # 返回全部结果
    result <- list(
      coefficients = if (inherits(x, "xts")) {
        xts::xts(coefficients, order.by = result_index)
      } else {
        coefficients
      },
      r.squared = if (inherits(x, "xts")) {
        xts(r_squared, order.by = result_index, name = paste0("r_squared_", n, "d"))
      } else {
        setNames(r_squared, paste0("r_squared_", n, "d"))
      },
      adj.r.squared = if (inherits(x, "xts")) {
        xts(adj_r_squared, order.by = result_index, name = paste0("adj_r_squared_", n, "d"))
      } else {
        setNames(adj_r_squared, paste0("adj_r_squared_", n, "d"))
      },
      f.statistic = if (inherits(x, "xts")) {
        xts(f_statistic, order.by = result_index, name = paste0("f_stat_", n, "d"))
      } else {
        setNames(f_statistic, paste0("f_stat_", n, "d"))
      },
      p.value = if (inherits(x, "xts")) {
        xts(p_value, order.by = result_index, name = paste0("p_value_", n, "d"))
      } else {
        setNames(p_value, paste0("p_value_", n, "d"))
      }
    )

    class(result) <- "runRegression"
    return(result)
  }
}

#' Print Method for runRegression Objects
#' runRegression对象的打印方法
#'
#' Prints a summary of the sliding window regression results.
#' 打印滑动窗口回归结果的摘要信息。
#'
#' @param x A runRegression object.
#'          一个runRegression对象。
#' @param ... Additional arguments passed to print.
#'            传递给print函数的其他参数。
#'
#' @return The input object is returned invisibly.
#'         输入对象以不可见方式返回。
#' @importFrom xts xts
#' @importFrom utils tail
#' @export
#' @method print runRegression
#'
#' @examples
#' # Generate sample data and run regression
#' # 生成示例数据并运行回归
#' set.seed(123)
#' x <- xts::xts(rnorm(100), order.by = Sys.Date() - 99:0)
#' reg_results <- runRegression(x, n = 10)
#' print(reg_results)
print.runRegression <- function(x, ...) {
  cat("Sliding Window Linear Regression Results (Window Size:", nrow(x$coefficients) + 1, ")\n")
  cat("Regression Terms:", paste(colnames(x$coefficients), collapse = ", "), "\n")
  cat("Number of Samples:", nrow(x$coefficients), "\n")

  # Display summary of R-squared statistics
  # 显示R²统计量摘要
  r2_mean <- mean(x$r.squared, na.rm = TRUE)
  r2_min <- min(x$r.squared, na.rm = TRUE)
  r2_max <- max(x$r.squared, na.rm = TRUE)
  cat(sprintf("R-squared Statistics: Mean=%.4f, Min=%.4f, Max=%.4f\n", r2_mean, r2_min, r2_max))

  # Display regression coefficients
  # 显示回归系数
  cat("\nRegression Coefficients:\n")
  print((x$coefficients))
  invisible(x)
}

#' Convert runRegression Results to Data Frame
#' 将runRegression结果转换为数据框
#'
#' Converts the results of a runRegression analysis into a data frame for easier manipulation.
#' 将runRegression分析的结果转换为数据框，便于进一步处理。
#'
#' @param reg_result A runRegression object.
#'                   一个runRegression对象。
#' @param prefix An optional prefix to add to column names.
#'               可选的列名前缀。
#'
#' @return A data frame containing regression results.
#'         包含回归结果的数据框。
#' @export
#'
#' @examples
#' # Generate sample data and run regression
#' # 生成示例数据并运行回归
#' set.seed(123)
#' x <- xts::xts(rnorm(100), order.by = Sys.Date() - 99:0)
#' reg_results <- runRegression(x, n = 10)
#'
#' # Convert to data frame
#' # 转换为数据框
#' df <- convertRegressionToDataframe(reg_results, prefix = "reg_10d")
convertRegressionToDataframe <- function(reg_result, prefix = "") {
  # Extract coefficients
  # 提取系数
  coef_df <- as.data.frame(reg_result$coefficients)
  coef_df$date <- zoo::index(reg_result$coefficients)

  # Extract other metrics
  # 提取其他指标
  metrics_df <- data.frame(
    date = index(reg_result$r.squared),
    r_squared = as.numeric(reg_result$r.squared),
    adj_r_squared = as.numeric(reg_result$adj.r.squared),
    f_statistic = as.numeric(reg_result$f.statistic),
    p_value = as.numeric(reg_result$p.value)
  )

  # Merge all data
  # 合并所有数据
  result_df <- merge(coef_df, metrics_df, by = "date")

  # Add prefix (if provided)
  # 添加前缀（如果提供）
  if (prefix != "") {
    colnames(result_df)[-1] <- paste0(prefix, "_", colnames(result_df)[-1])
  }

  # Sort by date
  # 按日期排序
  result_df <- result_df[order(result_df$date), ]

  return(result_df)
}
