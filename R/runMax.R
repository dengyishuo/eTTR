runMax <- function(x, n = 10, cumulative = FALSE) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x)) {
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  }

  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. runMax only supports univariate 'x'")
  }

  if (cumulative) {
    NAs <- sum(is.na(x))
    if (NAs > 0) {
      if (any(is.na(x[-(1:NAs)]))) stop("Series contains non-leading NAs")
      if (NAs + n > NROW(x)) stop("not enough non-NA values")
    }
    beg <- 1 + NAs
    result <- double(NROW(x))
    result[beg:NROW(x)] <- cummax(x[beg:NROW(x)])
    is.na(result) <- seq_len(n - 1 + NAs)
  } else {
    result <- .Call(runmax, x, n)
  }

  reclass(result, x)
}
