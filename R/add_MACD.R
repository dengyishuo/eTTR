#' @title Add Moving Average Convergence/Divergence (MACD)
#'
#' @description Computes the MACD oscillator and its signal line for each asset
#'   in a long-format panel data frame. The MACD is the difference (or
#'   percentage ratio) between a fast and a slow moving average of \code{close}.
#'   Results are appended as columns \code{macd} and \code{signal}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param nFast Integer. Fast moving average period. Defaults to \code{12}.
#' @param nSlow Integer. Slow moving average period. Defaults to \code{26}.
#' @param nSig Integer. Signal line smoothing period. Defaults to \code{9}.
#' @param maType Character or list. Moving average type(s). Defaults to
#'   \code{"EMA"}.
#' @param percent Logical. If \code{TRUE} (default), return MACD as a
#'   percentage ratio. If \code{FALSE}, return the absolute difference.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the moving average function.
#'
#' @return The input data frame with additional columns \code{macd} (the MACD
#'   oscillator) and \code{signal} (the signal line).
#'
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#'
#' @examples
#' data(ettr_stocks)
#' result <- add_MACD(ettr_stocks)
add_MACD <- function(mkt_data, nFast = 12, nSlow = 26, nSig = 9, maType,
                     percent = TRUE, append = TRUE,
                     output = c("tibble", "data.frame"), ...) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  if (missing(maType)) maType <- "EMA"

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }
  required_cols <- c("date", "code", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    x <- xts::xts(sub$close, order.by = as.Date(sub$date))

    ma_type <- maType
    if (is.list(ma_type)) {
      if (!all(sapply(ma_type, is.list)) || length(ma_type) != 3) {
        stop("If 'maType' is a list, you must specify *three* MAs")
      }
      if (!is.null(formals(ma_type[[1]][[1]])$n) && is.null(ma_type[[1]]$n)) ma_type[[1]]$n <- nFast
      if (!is.null(formals(ma_type[[2]][[1]])$n) && is.null(ma_type[[2]]$n)) ma_type[[2]]$n <- nSlow
      if (!is.null(formals(ma_type[[3]][[1]])$n) && is.null(ma_type[[3]]$n)) ma_type[[3]]$n <- nSig
      mavg.fast <- do.call(ma_type[[1]][[1]], c(list(x), ma_type[[1]][-1]))
      mavg.slow <- do.call(ma_type[[2]][[1]], c(list(x), ma_type[[2]][-1]))
    } else {
      mavg.fast <- do.call(ma_type, c(list(x), list(n = nFast, ...)))
      mavg.slow <- do.call(ma_type, c(list(x), list(n = nSlow, ...)))
    }

    if (percent) {
      macd_val <- 100 * (mavg.fast / mavg.slow - 1)
    } else {
      macd_val <- mavg.fast - mavg.slow
    }

    if (is.list(ma_type)) {
      signal_val <- do.call(ma_type[[3]][[1]], c(list(macd_val), ma_type[[3]][-1]))
    } else {
      signal_val <- do.call(ma_type, c(list(macd_val), list(n = nSig, ...)))
    }

    sub[["macd"]]   <- as.numeric(macd_val)
    sub[["signal"]] <- as.numeric(signal_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Column selection ───────────────────────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "macd", "signal"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
