#' @title Arnaud Legoux Moving Average (ALMA)
#' @description
#' Computes the Arnaud Legoux Moving Average for each security in a long-format
#' panel data frame. ALMA is a Gaussian-weighted moving average that reduces lag
#' while maintaining smoothness. The Gaussian window is controlled by \code{offset}
#' (center position) and \code{sigma} (spread), allowing flexible tuning between
#' responsiveness and noise reduction.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and \code{close}.
#' @param n Integer. Look-back window. Defaults to \code{9}.
#' @param offset Numeric. Percentile for the Gaussian weight center in
#'   \code{[0, 1]}. Higher values emphasize more recent prices. Defaults to
#'   \code{0.85}.
#' @param sigma Numeric. Standard deviation of the Gaussian distribution.
#'   Lower values narrow the bell curve and reduce smoothing. Defaults to
#'   \code{6}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{ALMA_\{n\}} (e.g., \code{ALMA_9}) containing
#'   the Gaussian-weighted moving average values.
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date  = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code  = rep(c("AAPL", "MSFT"), each = 60),
#'   name  = rep(c("Apple", "Microsoft"), each = 60),
#'   close = c(runif(60, 150, 200), runif(60, 300, 400))
#' )
#' # Example 1: Default parameters
#' result <- add_ALMA(mkt_data)
#' # Example 2: Slim output with larger window
#' result <- add_ALMA(mkt_data, n = 21, append = FALSE)
#' # Example 3: Less smoothing via lower sigma
#' result <- add_ALMA(mkt_data, n = 12, sigma = 4, output = "data.frame")
#' }
add_ALMA <- function(mkt_data, n = 9, offset = 0.85, sigma = 6, append = TRUE,
                     output = c("tibble", "data.frame")) {
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
  if (offset < 0 || offset > 1) stop("Please ensure 0 <= offset <= 1")
  if (sigma <= 0) stop("sigma must be > 0")

  # ── Pre-compute Gaussian weights (shared across all assets) ───────────────
  m <- floor(offset * (n - 1))
  s <- n / sigma
  wts <- exp(-((seq(0, n - 1) - m)^2) / (2 * s * s))
  sumWeights <- sum(wts)
  if (sumWeights != 0) wts <- wts / sumWeights

  # ── Split-apply-combine over each asset ───────────────────────────────────
  col_name <- paste0("ALMA_", n)
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    if (n > nrow(sub)) {
      warning(sprintf("Skipping code '%s': n = %d exceeds available rows (%d).", cd, n, nrow(sub)))
      sub[[col_name]] <- NA_real_
      return(sub)
    }

    close_xts <- xts::xts(sub$close, order.by = sub$date)
    alma <- WMA(close_xts, n = n, wts = wts)
    sub[[col_name]] <- as.numeric(alma)
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
