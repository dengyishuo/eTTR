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
#' Calculate and add KDJ indicators to stock data (with date column)
#' @description This function calculates KDJ indicators and ensures the result includes a date column.
#' @param mktdata xts object containing OHLCV data
#' @param n integer. KDJ calculation period (default 9)
#' @param m1 integer. Smoothing parameter for K (default 3)
#' @param m2 integer. Smoothing parameter for D (default 3)
#' @param fill_na_method character. Method to fill NA values: "none", "initial", "interpolate" (default "none")
#' @param append logical. Append KDJ columns to original data (default TRUE)
#' @param date_col character. Name of the date column (default "date")
#' @return Tibble with KDJ columns added, including a date column
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
#' @examples
#' \dontrun{
#' library(quantmod)
#' getSymbols("AAPL")
#' aapl_with_kdj <- add_kdj(AAPL, n = 9, m1 = 3, m2 = 3)
#' head(aapl_with_kdj$date) # Check date column
#' }
#' Calculate and add KDJ indicators to stock data (date column first)
#' @description This function calculates KDJ indicators and ensures the date column is always first.
#' @param mktdata xts object containing OHLCV data
#' @param n integer. KDJ calculation period (default 9)
#' @param m1 integer. Smoothing parameter for K (default 3)
#' @param m2 integer. Smoothing parameter for D (default 3)
#' @param fill_na_method character. Method to fill NA values: "none", "initial", "interpolate" (default "none")
#' @param append logical. Append KDJ columns to original data (default TRUE)
#' @param date_col character. Name of the date column (default "date")
#' @return Tibble with KDJ columns added, date column is always the first column
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols select
#' @importFrom zoo index
#' @export
#' @examples
#' \dontrun{
#' library(quantmod)
#' getSymbols("AAPL")
#' aapl_with_kdj <- add_kdj(AAPL, n = 9, m1 = 3, m2 = 3)
#' head(aapl_with_kdj)  # Date column should be first
#' }
add_kdj <- function(mktdata, n = 9, m1 = 3, m2 = 3,
                    fill_na_method = "none", append = TRUE,
                    date_col = "date") {
  # 验证输入
  stopifnot(
    inherits(mktdata, "xts"),
    all(c("High", "Low", "Close") %in% colnames(mktdata)),
    n > 0 & m1 > 0 & m2 > 0,
    fill_na_method %in% c("none", "initial", "interpolate")
  )

  # 从xts对象中提取日期
  dates <- zoo::index(mktdata)

  # 计算KDJ
  kdj_result <- KDJ(
    ohlc = mktdata,
    n = n,
    m1 = m1,
    m2 = m2,
    fill_na_method = fill_na_method
  )

  # 转换为tibble并确保日期列在第一列
  kdj_tbl <- tibble::as_tibble(kdj_result) %>%
    tibble::add_column(!!date_col := dates, .before = 1)

  # 返回结果
  if (append) {
    # 转换原始数据为tibble
    mktdata_tbl <- tibble::as_tibble(mktdata)

    # 合并数据，确保日期列在第一列
    result <- dplyr::bind_cols(
      tibble::tibble(!!date_col := dates),
      mktdata_tbl,
      kdj_tbl %>% dplyr::select(-!!date_col)
    )
  } else {
    result <- kdj_tbl
  }

  return(result)
}
