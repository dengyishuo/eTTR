#' Functions to create Technical Trading Rules (TTR)
#'
#' This package contains many of the most popular technical analysis functions,
#' as well as functions to retrieve U.S. stock symbols, and data from Yahoo
#' Finance.
#'
#' Users will probably be most interested in the following functions:\cr
#' \code{\link[TTR]{ADX}}\cr \code{\link[TTR]{BBands}}\cr \code{\link[TTR]{MACD}}\cr
#' \code{\link[TTR]{RSI}}\cr \code{\link[TTR]{stoch}}\cr \code{\link[TTR]{VWAP}}\cr
#'
#' Global import functions from external packages, effective for all package scripts
#' @keywords internal
#' @importFrom stats embed is.ts
#' @importFrom xts try.xts is.xts merge.xts last reclass
#' @importFrom zoo index coredata na.approx
#' @importFrom TTR SMA EMA WMA DEMA ZLEMA EVWMA TR SAR OBV stoch
#' @importFrom quantmod HLC Hi Lo
#' @export runPercentRank
#' @export runSum
#' @export wilderSum
#' @export runMAD
#' @export runSD
#' @export runVar
#' @name eTTR
#' @aliases eTTR-package
#' @author DengYishuo
#' @references The following sites were used to code/document this package:\cr
#' \url{https://www.fmlabs.com/reference/default.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/}\cr
#' \url{https://www.linnsoft.com/indicators}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators}\cr
#' @keywords package
"_PACKAGE"

# Declare all internal global variables to eliminate no visible binding notes
utils::globalVariables(c(
  "WilderSum",
  "runcov",
  "runmad",
  "runmax",
  "runmedian",
  "runmin",
  "runsum",
  "adjRatios",
  "ettr_zigzag",
  "ettr_rollPercentRank"
))
