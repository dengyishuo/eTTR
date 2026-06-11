#' Rolling Median Absolute Deviation (MAD)
#'
#' Compute rolling Median Absolute Deviation over a fixed-width moving window.
#' Supports cumulative mode and configurable MAD scaling constant for consistency with stats::mad.
#'
#' @param x Univariate time series vector, matrix or xts object.
#' @param n Integer rolling window size, must satisfy \code{1 <= n <= NROW(x)}. Default 10.
#' @param center Numeric vector/xts, precomputed central tendency series used for absolute deviation calculation.
#'   If \code{NULL}, rolling median of \code{x} with window \code{n} is used automatically. Default \code{NULL}.
#' @param stat Character string specifying central statistic for deviation aggregation; one of \code{c("median", "mean")}.
#'   \code{"median"} computes median absolute deviation; \code{"mean"} computes mean absolute deviation. Default \code{"median"}.
#' @param constant Numeric scaling constant to multiply raw MAD values (standard stats MAD constant = 1.4826). Default 1.4826.
#' @param non.unique Character string handling ties in window absolute deviations; one of \code{c("mean", "max", "min")}.
#'   \code{"mean"} average tied values; \code{"max"} pick maximum tie value; \code{"min"} pick minimum tie value. Default \code{"mean"}.
#' @param cumulative Logical flag, if \code{TRUE} compute expanding/cumulative window instead of rolling fixed window. Default \code{FALSE}.
#'
#' @details
#' Internal computation is implemented via compiled C function \code{runmad} for high performance.
#' Workflow:
#' 1. Coerce input to xts with \code{\link[xts]{try.xts}}.
#' 2. Validate window length and univariate input constraint.
#' 3. Auto-generate rolling median center series if no \code{center} input is provided.
#' 4. Map character configuration arguments to integer flags for C call interface.
#' 5. Execute fast rolling calculation via native C code.
#' 6. Apply standard MAD scaling constant when median-based deviation is selected.
#' 7. Revert output to original input class using \code{\link[xts]{reclass}}.
#'
#' @return Object with identical class as input \code{x}, containing rolling MAD values.
#'
#' @importFrom xts try.xts reclass
#' @export
runMAD <- function(x, n = 10, center = NULL, stat = "median",
                   constant = 1.4826, non.unique = "mean", cumulative = FALSE) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. runMAD only supports univariate 'x'")
  }

  if (is.null(center)) {
    center <- runMedian(x, n, cumulative = cumulative)
  }

  median_flag <- match.arg(stat, c("mean", "median"))
  median_flag <- switch(median_flag,
    median = TRUE,
    mean = FALSE
  )

  nu_flag <- match.arg(non.unique, c("mean", "max", "min"))
  nu_flag <- switch(nu_flag,
    mean = 0,
    max = 1,
    min = -1
  )

  result <- .Call(runmad, x, center, n, median_flag, nu_flag, cumulative)
  if (median_flag) result <- result * constant
  reclass(result, x)
}
