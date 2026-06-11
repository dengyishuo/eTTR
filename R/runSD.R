#' Rolling Standard Deviation
#'
#' Calculate rolling or cumulative standard deviation by taking square root of rolling covariance of the series with itself.
#'
#' @param x Univariate numeric time series, can be vector, matrix or xts/zoo object.
#' @param n Integer rolling window size, must satisfy \code{1 <= n <= NROW(x)}. Default 10.
#' @param sample Logical, whether to compute sample standard deviation (divide by n-1) or population standard deviation (divide by n). Default \code{TRUE}.
#' @param cumulative Logical, if \code{TRUE} use expanding cumulative window instead of fixed rolling window. Default \code{FALSE}.
#'
#' @details
#' Standard deviation is derived from rolling covariance:
#' \deqn{runSD(x) = \sqrt{runCov(x, x)}}
#' All input coercion, window validation, NA handling and xts class restoration logic are delegated to internal \code{\link{runCov}}.
#'
#' @return Object with the same class as input \code{x}, containing rolling standard deviation values.
#'
#' @export
runSD <- function(x, n = 10, sample = TRUE, cumulative = FALSE) {
  result <- sqrt(runCov(x, x, n, sample = sample, cumulative = cumulative))
  return(result)
}
