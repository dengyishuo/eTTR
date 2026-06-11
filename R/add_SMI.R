#' @title Add Stochastic Momentum Index (SMI)
#'
#' @description Computes the Stochastic Momentum Index and its signal line for
#'   each asset in a long-format panel data frame. The SMI measures how far the
#'   current close is from the midpoint of the recent high-low range, double-
#'   smoothed by fast and slow MAs. Results are appended as columns \code{SMI}
#'   and \code{signal}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, \code{low}, and
#'   \code{close}.
#' @param n Integer. Look-back window for the high-low range. Defaults to
#'   \code{13}.
#' @param nFast Integer. Fast smoothing period. Defaults to \code{2}.
#' @param nSlow Integer. Slow smoothing period. Defaults to \code{25}.
#' @param nSig Integer. Signal line period. Defaults to \code{9}.
#' @param maType Character or list. Moving average type(s). Defaults to
#'   \code{"EMA"}.
#' @param bounded Logical. If \code{TRUE} (default), the high-low range uses
#'   the current bar; if \code{FALSE}, it uses the previous bar.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the moving average function.
#'
#' @return The input data frame with additional columns \code{SMI} (the
#'   Stochastic Momentum Index) and \code{signal} (the signal line).
#'
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date  = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code  = rep(c("AAPL", "MSFT"), each = 60),
#'   name  = rep(c("Apple", "Microsoft"), each = 60),
#'   high  = c(runif(60, 160, 210), runif(60, 310, 410)),
#'   low   = c(runif(60, 140, 190), runif(60, 290, 390)),
#'   close = c(runif(60, 150, 200), runif(60, 300, 400))
#' )
#' # Example 1: Default parameters
#' result <- add_SMI(mkt_data)
#' # Example 2: Custom look-back window
#' result <- add_SMI(mkt_data, n = 9)
#' # Example 3: Slim output
#' result <- add_SMI(mkt_data, n = 13, append = FALSE)
#' }
add_SMI <- function(mkt_data, n = 13, nFast = 2, nSlow = 25, nSig = 9,
                    maType, bounded = TRUE, append = TRUE,
                    output = c("tibble", "data.frame"), ...) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  if (missing(maType)) maType <- "EMA"

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, high, low, close.")
  }
  required_cols <- c("date", "code", "high", "low", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    high  <- xts::xts(sub$high,  order.by = as.Date(sub$date))
    low   <- xts::xts(sub$low,   order.by = as.Date(sub$date))
    close <- xts::xts(sub$close, order.by = as.Date(sub$date))

    # Calculate high-low range boundaries
    if (bounded) {
      hmax <- runMax(high, n)
      lmin <- runMin(low, n)
    } else {
      hmax <- runMax(c(high[1], high[-NROW(high)]), n)
      lmin <- runMin(c(low[1],  low[-NROW(low)]),  n)
    }
    hmax <- ifelse(is.na(hmax), high, hmax)
    lmin <- ifelse(is.na(lmin), low,  lmin)

    hldiff <- hmax - lmin
    cdiff  <- close - (hmax + lmin) / 2

    ma_type <- maType
    if (is.list(ma_type)) {
      if (!all(sapply(ma_type, is.list)) || length(ma_type) != 3) {
        stop("maType list must contain three moving average configurations")
      }
      if (!is.null(formals(ma_type[[1]][[1]])$n) && is.null(ma_type[[1]]$n)) ma_type[[1]]$n <- nFast
      if (!is.null(formals(ma_type[[2]][[1]])$n) && is.null(ma_type[[2]]$n)) ma_type[[2]]$n <- nSlow
      if (!is.null(formals(ma_type[[3]][[1]])$n) && is.null(ma_type[[3]]$n)) ma_type[[3]]$n <- nSig
      num1 <- do.call(ma_type[[1]][[1]], c(list(cdiff),  ma_type[[1]][-1]))
      den1 <- do.call(ma_type[[1]][[1]], c(list(hldiff), ma_type[[1]][-1]))
      num2 <- do.call(ma_type[[2]][[1]], c(list(num1),   ma_type[[2]][-1]))
      den2 <- do.call(ma_type[[2]][[1]], c(list(den1),   ma_type[[2]][-1]))
      smi_val    <- 100 * (num2 / (den2 / 2))
      signal_val <- do.call(ma_type[[3]][[1]], c(list(smi_val), ma_type[[3]][-1]))
    } else {
      num1 <- do.call(ma_type, c(list(cdiff),  list(n = nSlow, ...)))
      den1 <- do.call(ma_type, c(list(hldiff), list(n = nSlow, ...)))
      num2 <- do.call(ma_type, c(list(num1),   list(n = nFast, ...)))
      den2 <- do.call(ma_type, c(list(den1),   list(n = nFast, ...)))
      smi_val    <- 100 * (num2 / (den2 / 2))
      signal_val <- do.call(ma_type, c(list(smi_val), list(n = nSig, ...)))
    }

    sub[["SMI"]]    <- as.numeric(smi_val)
    sub[["signal"]] <- as.numeric(signal_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Column selection ───────────────────────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "SMI", "signal"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
