#' @title Williams %R
#' @description
#' Computes Williams Percent R for each security in a long-format panel data
#' frame. Williams %R is a momentum indicator that measures the level of the
#' close relative to the highest high over a look-back window, expressed as a
#' proportion (0 to 1) or optionally scaled to -100 to 0.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price/volume columns.
#' @param n Integer. Look-back window. Defaults to \code{14}.
#' @param scale Logical. If \code{TRUE}, multiply the raw proportion by -100 to
#'   match the traditional Williams %R scale (-100 to 0). Defaults to
#'   \code{FALSE}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{WPR_\{n\}} (or \code{WPR_scaled} when
#'   \code{scale = TRUE}) containing the Williams %R values.
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_WPR(ettr_stocks)
add_WPR <- function(mkt_data, n = 14, scale = FALSE, append = TRUE,
                    output = c("tibble", "data.frame")) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, high, low, close.")
  }
  required_cols <- c("date", "code", "high", "low", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  col_name <- if (scale) "WPR_scaled" else paste0("WPR_", n)

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    high <- xts::xts(sub$high, order.by = as.Date(sub$date))
    low <- xts::xts(sub$low, order.by = as.Date(sub$date))
    close <- xts::xts(sub$close, order.by = as.Date(sub$date))

    hmax <- TTR::runMax(high, n)
    lmin <- TTR::runMin(low, n)
    pctR <- (hmax - close) / (hmax - lmin)
    pctR[is.nan(pctR)] <- 0.5

    if (scale) pctR <- -100 * pctR

    sub[[col_name]] <- as.numeric(pctR)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Column selection ───────────────────────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", col_name), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
