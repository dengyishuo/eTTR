#' @title Money Flow Index (MFI)
#' @description
#' Computes the Money Flow Index for each security in a long-format panel data
#' frame. MFI is a momentum oscillator that uses both price and volume to measure
#' buying and selling pressure, and is often called the volume-weighted RSI.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price/volume columns.
#' @param n Integer. Look-back window. Defaults to \code{14}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{MFI} containing values in the range
#'   \code{[0, 100]}.
#' @export
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_MFI(ettr_stocks)
add_MFI <- function(mkt_data, n = 14, append = TRUE, output = c("tibble", "data.frame")) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, high, low, close, volume.")
  }
  required_cols <- c("date", "code", "high", "low", "close", "volume")
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
    vol <- sub$volume

    # Typical price = mean of HLC
    tp <- rowMeans(hlc)
    tp_lag <- c(NA, tp[-length(tp)])

    mf <- tp * vol
    pmf <- ifelse(!is.na(tp) & !is.na(tp_lag) & tp > tp_lag, mf, 0)
    nmf <- ifelse(!is.na(tp) & !is.na(tp_lag) & tp < tp_lag, mf, 0)

    num <- TTR::runSum(pmf, n)
    den <- TTR::runSum(nmf, n)
    mr  <- num / den
    mfi <- 100 - (100 / (1 + mr))
    mfi[!is.na(den) & den == 0] <- 100
    mfi[!is.na(den) & !is.na(num) & den == 0 & num == 0] <- 50

    sub[["MFI"]] <- as.numeric(mfi)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original OHLCV columns ─────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "MFI"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Return in requested format ─────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
