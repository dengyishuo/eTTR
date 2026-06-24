#' US Tech Stock Daily OHLCV Panel Data
#'
#' Daily price and volume data for 5 major US technology stocks
#' (AAPL, TSLA, NVDA, MSFT, AMZN) from 2020-01-02 to 2024-12-30,
#' in long format suitable for use with all \code{add_*()} functions.
#'
#' @name ettr_stocks
#' @docType data
#' @format A data frame with 6285 rows and 9 columns:
#' \describe{
#'   \item{date}{Trading date (\code{Date}).}
#'   \item{code}{Ticker symbol (character): AAPL, TSLA, NVDA, MSFT, AMZN.}
#'   \item{name}{Company name (character).}
#'   \item{open}{Opening price (numeric).}
#'   \item{high}{Intraday high price (numeric).}
#'   \item{low}{Intraday low price (numeric).}
#'   \item{close}{Closing price (numeric).}
#'   \item{volume}{Trading volume in shares (numeric).}
#'   \item{adjusted}{Dividend and split adjusted closing price (numeric).}
#' }
#' @source Yahoo Finance via \code{quantmod::getSymbols()}.
#' @usage data(ettr_stocks)
#' @examples
#' data(ettr_stocks)
#' head(ettr_stocks)
#' table(ettr_stocks$code)
NULL
