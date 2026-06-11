#' Rolling Percent Rank
#'
#' Calculate rolling or cumulative percentile rank for univariate time series via compiled C backend.
#' Percent rank is adjusted by exact.multiplier to resolve ties in ranking windows.
#'
#' @param x Univariate numeric time series, supports vector, matrix or xts/zoo object.
#' @param n Integer rolling window length, must satisfy \code{1 <= n <= NROW(x)}. Default 260.
#' @param cumulative Logical, if \code{TRUE} compute expanding cumulative window; fixed rolling window when \code{FALSE}. Default \code{FALSE}.
#' @param exact.multiplier Numeric scalar between 0 and 1, adjustment factor for tied ranks in percentile calculation. Default 0.5.
#'
#' @details
#' Execution steps are as follows:
#' \itemize{
#'   \item Coerce input to xts format with \code{\link[xts]{try.xts}}.
#'   \item Validate input constraints: valid window range and exact.multiplier within \[0, 1\].
#'   \item NA value restriction: only leading NA values are allowed; non-leading NA triggers an error.
#'   \item Special optimization branch: non-cumulative window with \code{n = 1} directly returns a vector filled with exact.multiplier, skips C routine call.
#'   \item Core percentile ranking logic runs in native compiled C function \code{ettr_rollPercentRank}.
#'   \item Restore output to the original input data class with \code{\link[xts]{reclass}}.
#' }
#'
#' @return Object with identical class as input \code{x}, containing rolling percent rank values.
#'
#' @importFrom xts try.xts reclass
#' @export
runPercentRank <- function(x, n = 260, cumulative = FALSE, exact.multiplier = 0.5) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }
  if (exact.multiplier < 0 || exact.multiplier > 1) {
    stop(sprintf("exact.multiplier = %f is outside valid range: [0, 1]", exact.multiplier))
  }

  NAs <- sum(is.na(x))
  if (NAs > 0) {
    if (any(is.na(x[-(1:NAs)]))) stop("Series contains non-leading NAs")
  }

  if (!isTRUE(cumulative) && identical(as.integer(n), 1L)) {
    result <- double(NROW(x))
    result[] <- exact.multiplier
  } else {
    result <- .Call(
      ettr_rollPercentRank,
      x,
      n,
      isTRUE(cumulative),
      exact.multiplier
    )
  }

  reclass(result, x)
}
