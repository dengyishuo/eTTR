safe_na_check <- function(x, wts = NULL) {
  n <- length(x)
  if (n == 0) {
    return(TRUE)
  }

  # Handle case without weights
  if (is.null(wts)) {
    non_na <- which(!is.na(x))
    if (length(non_na) == 0) {
      return(TRUE)
    }

    first_valid <- min(non_na)
    if (first_valid == n) {
      return(FALSE)
    }

    # Check if there are any NAs after the first valid value
    return(any(is.na(x[(first_valid + 1):n])))
  }

  # Handle case with weights
  if (length(x) != length(wts)) {
    stop("x and wts must have the same length")
  }

  non_na_x <- which(!is.na(x))
  non_na_wts <- which(!is.na(wts))

  if (length(non_na_x) == 0 || length(non_na_wts) == 0) {
    return(TRUE)
  }

  first_x <- min(non_na_x)
  first_wts <- min(non_na_wts)
  common_start <- max(first_x, first_wts)

  if (common_start == n) {
    return(FALSE)
  }

  idx <- (common_start + 1):n
  # 修改这里：使用any()分别检查x和wts中的NA
  return(any(is.na(x[idx])) || any(is.na(wts[idx])))
}
