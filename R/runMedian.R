#' Rolling Median
#'
#' Compute rolling fixed-window or expanding cumulative median for univariate time series via compiled C backend.
#' Provides configurable tie-handling strategy for windows with even number of observations.
#'
#' @param x Univariate numeric time series, accepts vector, matrix or xts/zoo object.
#' @param n Integer rolling window length, must satisfy \code{1 <= n <= NROW(x)}. Default 10.
#' @param non.unique Character string specifying tie resolution method for even-sized windows;
#'   allowed values: \code{c("mean", "max", "min")}.
#'   \code{"mean"} = average of two middle values; \code{"max"} = upper middle value; \code{"min"} = lower middle value.
#'   Default \code{"mean"}.
#' @param cumulative Logical scalar. If \code{TRUE}, calculate expanding cumulative median; fixed rolling median when \code{FALSE}. Default \code{FALSE}.
#'
#' @details
#' 1. Input is coerced to xts with \code{\link[xts]{try.xts}}.
#' 2. Validate window bounds and restrict input to univariate series only.
#' 3. Map character \code{non.unique} option to integer flag for native C function interface.
#' 4. Core median calculation executed via compiled C routine \code{runmedian} through \code{.Call}.
#' 5. Restore output to the original input data class with \code{\link[xts]{reclass}}.
#'
#' @return Object with identical class as input \code{x}, containing rolling or cumulative median values.
#'
#' @importFrom xts try.xts reclass
#' @export
runMedian <- function(x, n = 10, non.unique = "mean", cumulative = FALSE) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. runMedian only supports univariate 'x'")
  }

  non.unique <- match.arg(non.unique, c("mean", "max", "min"))
  non.unique <- switch(non.unique,
    mean = 0L,
    max = 1L,
    min = -1L
  )

  result <- .Call(runmedian, x, n, non.unique, cumulative)
  reclass(result, x)
}
