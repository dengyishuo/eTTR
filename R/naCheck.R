naCheck <- function(x, n = 0) {
  # Ensure NAs are only at beginning of data.
  if (is.null(dim(x)[2])) {
    NAs <- sum(is.na(x))
    if (NAs > 0) {
      if (any(is.na(x[-(1:NAs)]))) stop("Series contains non-leading NAs")
    }
  } else {
    NAs <- sum(rowSums(is.na(x)) > 0)
    if (NAs > 0) {
      if (any(is.na(x[-(1:NAs), ]))) stop("Series contains non-leading NAs")
    }
  }

  res <- list()
  res$NAs <- NAs
  res$nonNA <- (1 + NAs):NROW(x)
  res$beg <- n + NAs

  invisible(res)
}
