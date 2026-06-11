#' Rolling Variance
#'
#' Calculate rolling or cumulative variance for a univariate time series, implemented as self-covariance via \code{\link{runCov}}.
#'
#' @param x Univariate numeric time series (vector, matrix, xts/zoo object), input series for variance calculation.
#' @param y Univariate numeric time series; if \code{NULL}, set equal to \code{x} to compute variance. Default \code{NULL}.
#' @param n Integer rolling window length, must satisfy \code{1 <= n <= NROW(x)}. Default 10.
#' @param sample Logical scalar. If \code{TRUE}, calculate sample variance (divisor \code{n-1});
#'   if \code{FALSE}, calculate population variance (divisor \code{n}). Default \code{TRUE}.
#' @param cumulative Logical scalar. If \code{TRUE}, use expanding cumulative window instead of fixed rolling window. Default \code{FALSE}.
#'
#' @details
#' Variance is mathematically equivalent to the covariance of a series with itself:
#' \deqn{runVar(x) = runCov(x, x)}
#' All input coercion, validation, NA handling and xts class restoration logic are delegated to \code{\link{runCov}}.
#'
#' @return Object with identical class as input \code{x}, containing rolling variance values.
#'
#' @export
runVar <- function(x, y = NULL, n = 10, sample = TRUE, cumulative = FALSE) {
  if (is.null(y)) y <- x
  result <- runCov(x, y, n, sample = sample, cumulative = cumulative)
  return(result)
}
