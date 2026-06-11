#' @title Add True Strength Index (TSI)
#'
#' @description Computes the True Strength Index and its signal line for each
#'   asset in a long-format panel data frame. TSI double-smooths the price
#'   momentum using two EMA passes and expresses it as a ratio to the
#'   double-smoothed absolute momentum, scaled to \code{[-100, 100]}. Results
#'   are appended as columns \code{TSI_<r>} and \code{TSIsig_<r>}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param r Integer. First (shorter) EMA smoothing period. Defaults to
#'   \code{13}.
#' @param s Integer. Second (longer) EMA smoothing period. Defaults to
#'   \code{25}.
#' @param signal_period Integer. Signal line EMA period. Defaults to \code{9}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to \code{EMA}.
#'
#' @return The input data frame with additional columns \code{TSI_<r>} (the
#'   True Strength Index) and \code{TSIsig_<r>} (the signal line).
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
#'   close = c(runif(60, 150, 200), runif(60, 300, 400))
#' )
#' # Example 1: Default parameters
#' result <- add_TSI(mkt_data)
#' # Example 2: Custom smoothing periods
#' result <- add_TSI(mkt_data, r = 10, s = 20)
#' # Example 3: Slim output
#' result <- add_TSI(mkt_data, r = 13, s = 25, append = FALSE)
#' }
add_TSI <- function(mkt_data, r = 13, s = 25, signal_period = 9, append = TRUE,
                    output = c("tibble", "data.frame"), ...) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }
  required_cols <- c("date", "code", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # ── Column names ──────────────────────────────────────────────────────────
  col_tsi <- paste0("TSI_", r)
  col_sig <- paste0("TSIsig_", r)

  # ── Split-apply-combine over each asset ───────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    min_rows <- r + s + signal_period + 1
    if (min_rows > nrow(sub)) {
      warning(sprintf("Skipping code '%s': insufficient rows for TSI computation.", cd))
      sub[[col_tsi]] <- NA_real_
      sub[[col_sig]] <- NA_real_
      return(sub)
    }

    close_xts <- xts::xts(sub$close, order.by = sub$date)

    # Price change and double-smoothed EMA
    price_change <- diff(as.numeric(close_xts))

    ema1     <- EMA(price_change, n = r, ...)
    ema2     <- EMA(ema1,         n = s, ...)
    abs_ema1 <- EMA(abs(price_change), n = r, ...)
    abs_ema2 <- EMA(abs_ema1,          n = s, ...)

    tsi_vec    <- (ema2 / abs_ema2) * 100
    signal_vec <- EMA(tsi_vec, n = signal_period, ...)

    # Pad one NA at front to align with original close length
    sub[[col_tsi]] <- c(NA_real_, tsi_vec)
    sub[[col_sig]] <- c(NA_real_, signal_vec)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Slim output when append = FALSE ───────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", col_tsi, col_sig), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format conversion ───────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
