#' @title Signal-to-Noise Ratio (SNR)
#' @description
#' Computes the Signal-to-Noise Ratio for each security in a long-format panel
#' data frame. SNR is defined as the absolute \code{n}-period price change
#' divided by the Average True Range (ATR) computed over the same \code{n}
#' periods. Higher values indicate that directional price movement dominates
#' random noise, making the instrument more suitable for trend-following
#' strategies. Required columns: \code{high}, \code{low}, \code{close}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, \code{low}, and
#'   \code{close}.
#' @param n Integer. Look-back window (required, no default). Used for both
#'   the price-change lag and the ATR smoothing period.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the \code{ATR} function (e.g.
#'   \code{maType}).
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{SNR_<n>} containing the signal-to-noise
#'   ratio values. Values are non-negative; larger values signal stronger
#'   directional price movement relative to intraday noise.
#' @export
#' @importFrom xts xts
#' @importFrom xts lag.xts
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date  = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code  = rep(c("AAPL", "MSFT"), each = 60),
#'   name  = rep(c("Apple", "Microsoft"), each = 60),
#'   high  = c(runif(60, 155, 205), runif(60, 305, 405)),
#'   low   = c(runif(60, 145, 195), runif(60, 295, 395)),
#'   close = c(runif(60, 150, 200), runif(60, 300, 400))
#' )
#' # Example 1: Window of 14 periods
#' result <- add_SNR(mkt_data, n = 14)
#' # Example 2: Longer window of 20 periods
#' result <- add_SNR(mkt_data, n = 20)
#' # Example 3: Slim output with n = 14
#' result <- add_SNR(mkt_data, n = 14, append = FALSE)
#' }
add_SNR <- function(mkt_data, n, append = TRUE, output = c("tibble", "data.frame"), ...) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }
  required_cols <- c("date", "code", "close", "high", "low")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # ── Split-apply-combine over each asset ───────────────────────────────────
  col_name <- paste0("SNR_", n)
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    if (n >= nrow(sub)) {
      warning(sprintf("Skipping code '%s': n = %d >= available rows (%d).", cd, n, nrow(sub)))
      sub[[col_name]] <- NA_real_
      return(sub)
    }

    hlc <- xts::xts(sub[, c("high", "low", "close")], order.by = sub$date)

    snr <- abs(hlc[, 3] - xts::lag.xts(hlc[, 3], n)) / ATR(hlc, n, ...)[, "atr"]
    sub[[col_name]] <- as.numeric(snr)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Slim output when append = FALSE ───────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", col_name), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format conversion ───────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
