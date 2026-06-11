#' Volatility Estimation
#'
#' Calculate various historical volatility estimators using closing prices or OHLC data.
#'
#' @param OHLC An xts object containing Open, High, Low, Close columns. Column names
#'   should be `"Open"`, `"High"`, `"Low"`, `"Close"` (case‑sensitive). If only a
#'   univariate series is provided, it is treated as closing prices.
#' @param n Window length for the volatility estimate. Default 10.
#' @param calc Type of volatility estimator: `"close"`, `"garman.klass"`, `"parkinson"`,
#'   `"rogers.satchell"`, `"gk.yz"`, or `"yang.zhang"`. Default `"close"`.
#' @param N Number of periods in a year (annualization factor). Default 260.
#' @param mean0 Logical. If `TRUE`, use a mean of 0 when calculating close‑to‑close
#'   volatility (alternative formula). Only relevant for `calc = "close"`. Default `FALSE`.
#' @param ... Additional arguments (e.g., `alpha` and `k` for `yang.zhang`).
#'
#' @return An xts object of the same length as the input containing the volatility
#'   estimate, expressed in annualized terms (unless `N = 1`).
#'
#' @importFrom xts try.xts is.xts lag.xts reclass
#' @importFrom zoo index
#' @export
volatility <- function(OHLC, n = 10, calc = "close", N = 260, mean0 = FALSE, ...) {
  OHLC <- try.xts(OHLC, error = as.matrix)

  # Choose an arg name that doesn't clash with ROC's 'type' arg
  calc <- match.arg(
    calc,
    c(
      "close", "garman.klass", "parkinson",
      "rogers.satchell", "gk.yz", "yang.zhang"
    )
  )

  # s       Volatility
  # N       Number of closing prices in a year
  # n       Number of historical prices used for the volatility estimate
  # ci      The closing price on the ith day
  # ri      Log return on the ith day

  # Historical Close-to-Close Volatility
  # http://www.sitmo.com/eq/172
  if (calc == "close") {
    # Add univariate case from Cedrick Johnson's R-SIG-Finance post
    if (NCOL(OHLC) == 1) {
      r <- ROC(OHLC[, 1], 1, ...)
    } else {
      r <- ROC(OHLC[, 4], 1, ...)
    }
    if (isTRUE(mean0)) {
      # This is an alternative SD calculation using an effective mean of 0
      s <- sqrt(N) * sqrt(runSum(r^2, n - 1) / (n - 2))
    } else {
      # This is the standard SD calculation using the sample mean
      s <- sqrt(N) * runSD(r, n - 1)
    }
  }

  # Historical Open-High-Low-Close Volatility: Garman Klass
  # http://www.sitmo.com/eq/402
  if (calc == "garman.klass") {
    s <- sqrt(N / n * runSum(.5 * log(OHLC[, 2] / OHLC[, 3])^2 -
      (2 * log(2) - 1) * log(OHLC[, 4] / OHLC[, 1])^2, n))
  }

  if (calc == "parkinson") {
    # Historical High-Low Volatility: Parkinson
    # http://www.sitmo.com/eq/173
    s <- sqrt(N / (4 * n * log(2)) * runSum(log(OHLC[, 2] / OHLC[, 3])^2, n))
  }

  if (calc == "rogers.satchell") {
    # Historical Open-High-Low-Close Volatility: Rogers Satchell
    # http://www.sitmo.com/eq/414
    s <- sqrt(N / n * runSum(
      log(OHLC[, 2] / OHLC[, 4]) * log(OHLC[, 2] / OHLC[, 1]) +
        log(OHLC[, 3] / OHLC[, 4]) * log(OHLC[, 3] / OHLC[, 1]), n
    ))
  }

  if (calc == "gk.yz") {
    # if( calc=="garman.klass.yang.zhang" ) {
    # Historical Open-High-Low-Close Volatility: Garman and Klass (Yang Zhang)
    # http://www.sitmo.com/eq/409
    if (is.xts(OHLC)) {
      Cl1 <- lag.xts(OHLC[, 4])
    } else {
      Cl1 <- c(NA, OHLC[-NROW(OHLC), 4])
    }
    s <- sqrt(N / n * runSum(
      log(OHLC[, 1] / Cl1)^2 +
        .5 * log(OHLC[, 2] / OHLC[, 3])^2 -
        (2 * log(2) - 1) * log(OHLC[, 4] / OHLC[, 1])^2, n
    ))

    # s <- sqrt( Z/n * runSum(
    #          log(op/cl[-1])^2 +
    #          .5*log(hi/lo)^2 -
    #          (2*log(2)-1)*log(cl/op)^2 ) )
  }

  if (calc == "yang.zhang") {
    # Historical Open-High-Low-Close Volatility: Yang Zhang
    # http://www.sitmo.com/eq/417
    if (is.xts(OHLC)) {
      Cl1 <- lag.xts(OHLC[, 4])
    } else {
      Cl1 <- c(NA, OHLC[-NROW(OHLC), 4])
    }

    dots <- list(...)
    if (is.null(dots$alpha)) {
      alpha <- 1.34
    }
    if (is.null(dots$k)) {
      k <- (alpha - 1) / (alpha + (n + 1) / (n - 1))
    }

    s2o <- N * runVar(log(OHLC[, 1] / Cl1), n = n)
    s2c <- N * runVar(log(OHLC[, 4] / OHLC[, 1]), n = n)
    s2rs <- volatility(OHLC = OHLC, n = n, calc = "rogers.satchell", N = N, ...)
    s <- sqrt(s2o + k * s2c + (1 - k) * (s2rs^2))
  }

  reclass(s, OHLC)
}
