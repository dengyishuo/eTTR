#' @title Ultimate Oscillator
#' @description
#' Computes the Ultimate Oscillator for each security in a long-format panel
#' data frame. The Ultimate Oscillator combines three time frames of buying
#' pressure relative to the true range, using weighted averages. Developed by
#' Larry Williams.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price/volume columns.
#' @param n Integer vector of length 3. Look-back windows for the three time
#'   frames. Defaults to \code{c(7, 14, 28)}.
#' @param wts Numeric vector of length 3. Weights applied to each time frame.
#'   Defaults to \code{c(4, 2, 1)}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{ultimateOscillator} containing values scaled
#'   between 0 and 100.
#' @export
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_ultimateOscillator(ettr_stocks)
add_ultimateOscillator <- function(mkt_data, n = c(7, 14, 28), wts = c(4, 2, 1),
                                   append = TRUE, output = c("tibble", "data.frame")) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  if (length(n) != 3 || length(wts) != 3) {
    stop("length(n) and length(wts) must both be 3.")
  }

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

    hlc <- cbind(sub$high, sub$low, sub$close)

    # True low: min(low, prior close); True high: max(high, prior close)
    prior_close <- c(NA, hlc[-nrow(hlc), 3])
    true_low  <- pmin(hlc[, 2], prior_close)
    true_high <- pmax(hlc[, 1], prior_close)
    tr        <- true_high - true_low
    bp        <- hlc[, 3] - true_low  # buying pressure

    osc <- bp * 0.0
    for (i in seq_along(n)) {
      osc <- osc + wts[i] * (TTR::runSum(bp, n[i]) / TTR::runSum(tr, n[i]))
    }
    osc <- 100.0 * osc / sum(wts)

    sub[["ultimateOscillator"]] <- as.numeric(osc)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original OHLCV columns ─────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "ultimateOscillator"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Return in requested format ─────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
