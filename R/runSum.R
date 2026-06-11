#' Rolling Sum
#'
#' Calculate fixed rolling window sum or expanding cumulative sum for univariate time series.
#' Rolling computation uses fast compiled C routine; cumulative mode relies on base \code{\link[base]{cumsum}}.
#'
#' @param x Univariate numeric time series, supports vector, matrix or xts/zoo object.
#' @param n Integer window length, must satisfy \code{1 <= n <= NROW(x)}. Default 10.
#' @param cumulative Logical. If \code{TRUE}, compute expanding cumulative sum; fixed rolling sum when \code{FALSE}. Default \code{FALSE}.
#'
#' @details
#' Execution workflow:
#' 1. Coerce input to xts with \code{\link[xts]{try.xts}}.
#' 2. Validate window range and restrict input to univariate series only.
#' 3. Two calculation branches:
#' \itemize{
#'   \item Rolling (cumulative = FALSE): High-performance sum via native C function \code{runsum} using \code{.Call}.
#'   \item Cumulative (cumulative = TRUE): Only leading NA values are permitted.
#'         Fill NA for initial incomplete window rows, compute cumulative sum over valid non-NA segment.
#' }
#' 4. Convert output back to original input class via \code{\link[xts]{reclass}}.
#'
#' @section Cumulative Mode NA Constraints:
#' Cumulative sum logic rejects any non-leading missing values.
#' If the count of leading NAs plus window length exceeds total rows, an insufficient data error is thrown.
#'
#' @return Object with identical class as input \code{x}, containing rolling or cumulative sum values.
#'
#' @importFrom xts try.xts reclass
#' @export
runSum <- function(x, n = 10, cumulative = FALSE) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. runSum only supports univariate 'x'")
  }

  if (cumulative) {
    NAs <- sum(is.na(x))
    if (NAs > 0) {
      if (any(is.na(x[-(1:NAs)]))) stop("Series contains non-leading NAs")
      if (NAs + n > NROW(x)) stop("not enough non-NA values")
    }
    beg <- 1 + NAs
    result <- double(NROW(x))
    result[beg:NROW(x)] <- cumsum(x[beg:NROW(x)])
    is.na(result) <- seq_len(n - 1 + NAs)
  } else {
    result <- .Call(runsum, x, n)
  }

  reclass(result, x)
}
