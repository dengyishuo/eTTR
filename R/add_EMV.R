#' @title Ease of Movement (EMV)
#' @description
#' Computes the Ease of Movement indicator for each security in a long-format
#' panel data frame. EMV relates price change to volume to identify how easily a
#' security moves in price. Developed by Richard Arms.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price/volume columns.
#' @param n Integer. Look-back window for the moving average of raw EMV. Defaults
#'   to \code{9}.
#' @param maType Character. Moving average type applied to raw EMV. Defaults to
#'   \code{"SMA"}.
#' @param vol.divisor Numeric. Divisor applied to volume to scale EMV values.
#'   Defaults to \code{10000}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the \code{maType} function.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with columns:
#'   \describe{
#'     \item{emv}{Raw Ease of Movement value.}
#'     \item{maEMV}{Smoothed Ease of Movement using \code{maType} over \code{n}
#'       periods.}
#'   }
#' @export
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date   = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code   = rep(c("AAPL", "MSFT"), each = 60),
#'   name   = rep(c("Apple", "Microsoft"), each = 60),
#'   high   = c(runif(60, 155, 205), runif(60, 305, 405)),
#'   low    = c(runif(60, 145, 195), runif(60, 295, 395)),
#'   close  = c(runif(60, 150, 200), runif(60, 300, 400)),
#'   volume = c(runif(60, 1e6, 2e6), runif(60, 5e5, 1.5e6))
#' )
#' # Example 1: Default parameters
#' result <- add_EMV(mkt_data)
#' # Example 2: Custom window
#' result <- add_EMV(mkt_data, n = 14)
#' # Example 3: Slim output
#' result <- add_EMV(mkt_data, n = 9, append = FALSE)
#' }
add_EMV <- function(mkt_data, n = 9, maType = "SMA", vol.divisor = 10000,
                    append = TRUE, output = c("tibble", "data.frame"), ...) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, high, low, volume.")
  }
  required_cols <- c("date", "code", "high", "low", "volume")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    h <- sub$high
    l <- sub$low
    vol <- sub$volume / vol.divisor

    mid <- (h + l) / 2
    mid_prev <- c(NA, mid[-length(mid)])
    emv_raw <- (mid - mid_prev) / (vol / (h - l))

    ma_args <- list(n = n, ...)
    ma_emv <- do.call(maType, c(list(emv_raw), ma_args))

    sub[["emv"]] <- as.numeric(emv_raw)
    sub[["maEMV"]] <- as.numeric(ma_emv)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original OHLCV columns ─────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "emv", "maEMV"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Return in requested format ─────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
