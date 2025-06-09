#' Functions to create Technical Trading Rules (TTR)
#'
#' This package contains many of the most popular technical analysis functions,
#' as well as functions to retrieve U.S. stock symbols, and data from Yahoo
#' Finance.
#'
#' Users will probably be most interested in the following functions:\cr
#' \code{\link{ADX}}\cr \code{\link{BBands}}\cr \code{\link{MACD}}\cr
#' \code{\link{RSI}}\cr \code{\link{stoch}}\cr \code{\link{VWAP}}\cr
#' @name eTTR
#' @aliases eTTR-package
#' @author DengYishuo
#' @references The following sites were used to code/document this package:\cr
#' \url{https://www.fmlabs.com/reference/default.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/}\cr
#' \url{https://www.linnsoft.com/indicators}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators}\cr
#' @keywords package
#' @examples
#' data(TSLA)
#' data(AAPL)
#' # Bollinger Bands
#' bbands <- BBands(TSLA[, c("High", "Low", "Close")])
#' # Directional Movement Index
#' adx <- ADX(TSLA[, c("High", "Low", "Close")])
#' # Moving Averages
#' ema <- EMA(TSLA[, "Close"], n = 20)
#' sma <- SMA(TSLA[, "Close"], n = 20)
#' # MACD
#' macd <- MACD(AAPL[, "Close"])
#' # RSI
#' rsi <- RSI(AAPL[, "Close"])
#' # Stochastics
#' stochOsc <- stoch(AAPL[, c("High", "Low", "Close")])
"_PACKAGE"
