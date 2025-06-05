#' Tesla Inc. Stock Price Data
#'
#' Daily stock prices for Tesla Inc. (TSLA) from 2010 to present.
#' @name TSLA
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
#' @keywords TSLA
#' @export
#' @examples
#' data(TSLA)
#' plot(tail(TSLA[, "Close"], 100), type = "l")
#' @rdname TSLA
NULL
