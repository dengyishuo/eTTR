runMean <- function(x, n = 10, cumulative = FALSE) {
  if (cumulative) {
    x.na <- sum(is.na(x))
    denom <- c(rep(NA_real_, x.na), seq_len(NROW(x) - x.na))
    result <- runSum(x, n, cumulative) / denom
  } else {
    result <- runSum(x, n) / n
  }
  return(result)
}
