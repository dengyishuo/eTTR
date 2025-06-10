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
#' @title Apple Inc. Stock Price Data (2010-Present)
#' @description
#' Historical daily stock prices for Apple Inc. (NASDAQ: AAPL),including open,
#' high, low, close prices and trading volume. Data is sourced from Yahoo Finance
#' and updated regularly.
#' @name AAPL
#' @docType data
#' @format A data frame with `r nrow(AAPL)` observations and 5 variables:
#' \describe{
#'   \item{Date}{Trading date (YYYY-MM-DD format)}
#'   \item{Open}{Opening price of the stock on the given day}
#'   \item{High}{Highest price reached during the trading day}
#'   \item{Low}{Lowest price reached during the trading day}
#'   \item{Close}{Closing price of the stock on the given day}
#'   \item{Volume}{Total number of shares traded during the day}
#' }
#' @source \url{https://finance.yahoo.com/quote/AAPL}
#' @keywords datasets finance stocks AAPL
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(AAPL)
#' # Plot closing prices for the last 100 trading days
#' plot(tail(AAPL$Close, 100),
#'   type = "l",
#'   main = "AAPL Closing Prices (Last 100 Days)",
#'   xlab = "Trading Day",
#'   ylab = "Price (USD)"
#' )
#' # Calculate 30-day moving average
#' ma30 <- SMA(AAPL$Close, n = 30)
#' }
NULL
