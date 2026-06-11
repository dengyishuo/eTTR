#' Momentum
#'
#' Calculate the momentum indicator, defined as the difference between the current
#' price and the price `n` periods ago.
#'
#' @param x Price series (numeric vector, matrix, or xts object).
#' @param n Number of periods for the momentum calculation. Default 1.
#' @param na.pad Logical. If `TRUE`, pad the result with NAs at the beginning. Default `TRUE`.
#'
#' @return An object of the same class as `x` containing the momentum values.
#'
#' @importFrom xts try.xts is.xts reclass
#' @export
momentum <- function(x, n = 1, na.pad = TRUE) {
  # Momentum
  # http://www.fmlabs.com/reference/Momentum.htm
  # https://www.metastock.com/Customer/Resources/TAAZ/?p=95
  # https://www.linnsoft.com/tour/techind/momentum.htm

  x <- try.xts(x, error = as.matrix)
  if (is.xts(x)) {
    mom <- diff(x, n, na.pad = na.pad)
  } else {
    NAs <- NULL
    if (na.pad) {
      NAs <- rep(NA, n)
    }
    mom <- c(NAs, diff(x, n))
  }
  reclass(mom, x)
}
