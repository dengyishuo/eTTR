#' Rolling Correlation Coefficient
#'
#' Calculate rolling or cumulative Pearson correlation between two time series.
#' Derived from rolling covariance divided by product of rolling standard deviations.
#'
#' @param x Numeric time series (vector, matrix, xts/zoo object), first variable for correlation calculation.
#' @param y Numeric time series (vector, matrix, xts/zoo object), second variable for correlation calculation.
#' @param n Integer rolling window length, must satisfy \code{1 <= n <= NROW(x)}. Default 10.
#' @param sample Logical, use sample variance/covariance divisor (n-1) if TRUE; population divisor (n) if FALSE. Default \code{TRUE}.
#' @param cumulative Logical, if \code{TRUE} compute expanding cumulative window instead of fixed rolling window. Default \code{FALSE}.
#'
#' @details
#' Pearson rolling correlation formula:
#' \deqn{runCor(x,y) = \frac{runCov(x,y)}{runSD(x) \times runSD(y)}}
#' All input validation, xts coercion, NA handling and index alignment are delegated to \code{\link{runCov}} and \code{\link{runSD}}.
#'
#' @return Object with identical class as input \code{x}, containing rolling correlation values ranging between -1 and 1.
#'
#' @export
runCor <- function(x, y, n = 10, sample = TRUE, cumulative = FALSE) {
  result <- runCov(x, y, n, sample = sample, cumulative = cumulative) /
    (runSD(x, n, sample = sample, cumulative) * runSD(y, n, sample = sample, cumulative))
  return(result)
}
