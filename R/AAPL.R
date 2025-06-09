#' @title Apple Inc. Stock Price Data (2010-Present)
#' @description
#' Historical daily stock prices for Apple Inc. (NASDAQ: AAPL),
#' including open, high, low, close prices and trading volume.
#' Data is sourced from Yahoo Finance and updated regularly.
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
