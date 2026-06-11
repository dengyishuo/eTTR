#' Wilder's Smoothed Rolling Sum
#'
#' Compute Wilder's smoothed moving sum, a recursive rolling average algorithm widely used in technical indicators such as RSI and ATR.
#' Calculation is powered by compiled C backend for high performance.
#'
#' @param x Univariate numeric time series, supports vector, matrix or xts/zoo object.
#' @param n Integer smoothing window length, must satisfy \code{1 <= n <= NROW(x)}. Default 10.
#'
#' @details
#' \itemize{
#'   \item Coerce input to xts format with \code{\link[xts]{try.xts}}.
#'   \item Validate window range and restrict input to univariate series only.
#'   \item Execute NA validity check via internal helper function \code{naCheck}.
#'   \item Core Wilder smoothed sum calculation runs through native compiled C function \code{WilderSum}.
#'   \item Restore output to the original input data class with \code{\link[xts]{reclass}}.
#' }
#' Wilder's smoothing formula recursively updates values instead of equal-weight rolling sum:
#' \deqn{WS_t = \frac{WS_{t-1} \times (n-1) + x_t}{n}}
#'
#' @return Object with identical class as input \code{x}, containing Wilder's smoothed rolling sum values.
#'
#' @importFrom xts try.xts reclass
#' @export
wilderSum <- function(x, n = 10) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. wilderSum only supports univariate 'x'")
  }

  naCheck(x, n) # Assume naCheck is a helper function
  result <- .Call(WilderSum, x, n)
  reclass(result, x)
}
