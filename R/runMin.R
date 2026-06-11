#' Rolling Minimum Value
#'
#' Calculate rolling fixed-window minimum or expanding cumulative minimum for univariate time series.
#' Rolling mode uses high-performance compiled C implementation; cumulative mode uses base \code{\link[base]{cummin}}.
#'
#' @param x Univariate numeric time series, supports vector, matrix or xts/zoo object.
#' @param n Integer rolling window size, must satisfy \code{1 <= n <= NROW(x)}. Default 10.
#' @param cumulative Logical, if \code{TRUE} compute expanding cumulative minimum; fixed rolling minimum when \code{FALSE}. Default \code{FALSE}.
#'
#' @details
#' Workflow:
#' 1. Coerce input to xts format with \code{\link[xts]{try.xts}}.
#' 2. Validate window range and enforce univariate input constraint.
#' 3. Two computation branches:
#' \itemize{
#'   \item Non-cumulative (rolling): Fast calculation via native C function \code{runmin} using \code{.Call}.
#'   \item Cumulative: Check only leading NAs are allowed, fill NA for initial incomplete window rows,
#'         compute cumulative minimum with \code{cummin} on valid non-NA tail segment.
#' }
#' 4. Restore output to original input class via \code{\link[xts]{reclass}}.
#'
#' @section NA Restriction for Cumulative Mode:
#' Cumulative minimum only permits leading missing values. Any non-leading NA will throw an error.
#' If total leading NAs plus window length exceed total rows, insufficient valid data error is raised.
#'
#' @return Object with identical class as input \code{x}, containing rolling/cumulative minimum values.
#'
#' @importFrom xts try.xts reclass
#' @export
runMin <- function(x, n = 10, cumulative = FALSE) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. runMin only supports univariate 'x'")
  }

  if (cumulative) {
    NAs <- sum(is.na(x))
    if (NAs > 0) {
      if (any(is.na(x[-(1:NAs)]))) stop("Series contains non-leading NAs")
      if (NAs + n > NROW(x)) stop("not enough non-NA values")
    }
    beg <- 1 + NAs
    result <- double(NROW(x))
    result[beg:NROW(x)] <- cummin(x[beg:NROW(x)])
    is.na(result) <- seq_len(n - 1 + NAs)
  } else {
    result <- .Call(runmin, x, n)
  }

  reclass(result, x)
}
