#' Rolling Covariance
#'
#' Compute rolling or cumulative covariance between two univariate time series via compiled C backend.
#' Supports both xts time series and regular numeric vectors/matrices as input.
#'
#' @param x Univariate numeric time series (vector, matrix, xts/zoo object), first input series.
#' @param y Univariate numeric time series (vector, matrix, xts/zoo object), second input series.
#' @param n Integer rolling window length, must satisfy \code{1 <= n <= NROW(x)}. Default 10.
#' @param sample Logical scalar. If \code{TRUE}, calculate sample covariance (divisor \code{n-1});
#'   if \code{FALSE}, calculate population covariance (divisor \code{n}). Default \code{TRUE}.
#' @param cumulative Logical scalar. If \code{TRUE}, use expanding cumulative window instead of fixed rolling window. Default \code{FALSE}.
#'
#' @details
#' 1. Coerce both input series to xts with \code{\link[xts]{try.xts}}.
#' 2. Combine two series into a two-column matrix; preserve xts index if both inputs are xts.
#' 3. Validate window size range and enforce univariate constraint for x and y.
#' 4. Fast covariance computation is delegated to native C function \code{runcov} via \code{.Call}.
#' 5. Restore original input data class with \code{\link[xts]{reclass}} before returning output.
#'
#' @return Object with identical class as input \code{x}, containing rolling covariance values.
#'
#' @importFrom xts try.xts reclass
#' @export
runCov <- function(x, y, n = 10, sample = TRUE, cumulative = FALSE) {
  x <- try.xts(x, error = as.matrix)
  y <- try.xts(y, error = as.matrix)
  if (is.xts(x) && is.xts(y)) {
    xy <- cbind(x, y)
  } else {
    xy <- cbind(as.vector(x), as.vector(y))
  }

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1 || NCOL(y) > 1) {
    stop("runCov only supports univariate 'x' and 'y'")
  }

  result <- .Call(runcov, xy[, 1], xy[, 2], n, sample, cumulative)
  reclass(result, x)
}
