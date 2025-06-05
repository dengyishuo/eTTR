#' Apple Inc. Stock Price Data
#'
#' Daily stock prices for Apple Inc. (AAPL) from 2010 to present.
#' @name AAPL
#' @docType data
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{Open}{Opening price}
#'   \item{High}{Highest price}
#'   \item{Low}{Lowest price}
#'   \item{Close}{Closing price}
#'   \item{Volume}{Trading volume}
#' }
#' @source Yahoo Finance
#' @keywords AAPL
#' @examples
#' data(AAPL)
#' plot(tail(AAPL[, "Close"], 100), type = "l")
#' @rdname AAPL
NULL
