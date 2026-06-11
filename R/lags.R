#' Calculate lags of a series
#'
#' @param x Object that is coercible to xts or matrix.
#' @param n Number of periods to lag (default is 1).
#' @param ... Further arguments to be passed from or to other methods.
#' @return A matrix of lagged values with columns labeled by original names and lag periods.
#' @note This function is deprecated in favor of xts::lag.xts and quantmod::Lag.
#' @author DengYishuo
#' @keywords ts
#' @export
#' @examples
#' data <- 1:10
#' lags(data, 2)
lags <- function(x, n = 1, ...) {
  # .Deprecated(c("xts::lag.xts","quantmod::Lag"),"eTTR")

  # Calculate lags of a series
  x <- as.matrix(x)
  if (is.null(colnames(x))) colnames(x) <- paste("V", 1:NCOL(x), sep = "")

  out <- embed(x, n + 1)
  if (n == 1) lag.names <- 1 else if (NCOL(x) == 1) lag.names <- 1:n else lag.names <- rep(1:n, NCOL(x))

  colnames(out) <- c(colnames(x), paste(colnames(x), sort(lag.names), sep = "."))

  return(out)
}
