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
#' @title Calculate and add KDJ indicators to stock data (with date column)
#' @description This function calculates KDJ indicators and ensures the result includes a date column.
#' @param mktdata xts object containing OHLCV data
#' @param n integer. KDJ calculation period (default 9)
#' @param m1 integer. Smoothing parameter for K (default 3)
#' @param m2 integer. Smoothing parameter for D (default 3)
#' @param fill_na_method character. Method to fill NA values: "none", "initial", "interpolate" (default "none")
#' @param append logical. Append KDJ columns to original data (default TRUE)
#' @param date_col character. Name of the date column (default "date")
#' @return Tibble with KDJ columns added, including a date column
#' @importFrom tibble as_tibble add_column tibble
#' @export
#' @examples
#' \dontrun{
#' data("AAPL") # Load example data
#' aapl_with_kdj <- add_kdj(AAPL, n = 9, m1 = 3, m2 = 3)
#' head(aapl_with_kdj$date) # Check date column
#' }
add_kdj <- function(mktdata, n = 9, m1 = 3, m2 = 3,
                    fill_na_method = "none", append = TRUE,
                    date_col = "date") {
  # Validate input parameters
  stopifnot(
    inherits(mktdata, "xts"),
    all(c("High", "Low", "Close") %in% colnames(mktdata)),
    n > 0 & m1 > 0 & m2 > 0,
    fill_na_method %in% c("none", "initial", "interpolate")
  )

  # Extract date index
  dates <- zoo::index(mktdata)

  # Calculate KDJ indicators
  kdj_result <- KDJ(
    ohlc = mktdata,
    n = n,
    m1 = m1,
    m2 = m2,
    fill_na_method = fill_na_method
  )

  # Convert KDJ results to tibble and add date column
  kdj_tbl <- tibble::as_tibble(kdj_result)
  kdj_tbl <- tibble::add_column(kdj_tbl, date = dates, .before = 1)
  names(kdj_tbl)[1] <- date_col

  # Determine return format based on append parameter
  if (append) {
    # Convert original data to tibble
    mktdata_tbl <- tibble::as_tibble(mktdata)

    # Create date column data frame
    date_df <- tibble::tibble(date = dates)
    names(date_df)[1] <- date_col

    # Merge data
    kdj_cols <- setdiff(names(kdj_tbl), date_col)
    result <- cbind(date_df, mktdata_tbl, kdj_tbl[, kdj_cols])
  } else {
    result <- kdj_tbl
  }

  return(result)
}
