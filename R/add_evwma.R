#' @title Add Elastic Volume Weighted Moving Average (EVWMA)
#'
#' @description Computes an Elastic Volume Weighted Moving Average for each
#'   asset in a long-format panel data frame and appends the result as a new
#'   column named \code{EVWMA}. The EVWMA uses cumulative volume as its
#'   smoothing factor, making it more responsive to periods of high trading
#'   activity.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{close}, and \code{volume}.
#' @param n Integer. Look-back window. Defaults to \code{10}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return The input data frame with an additional column \code{EVWMA}
#'   containing the elastic volume weighted moving average.
#'
#' @export
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date   = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code   = rep(c("AAPL", "MSFT"), each = 60),
#'   name   = rep(c("Apple", "Microsoft"), each = 60),
#'   close  = c(runif(60, 150, 200), runif(60, 300, 400)),
#'   volume = c(runif(60, 1e6, 5e6), runif(60, 2e6, 8e6))
#' )
#' # Example 1: Default parameters
#' result <- add_EVWMA(mkt_data)
#' # Example 2: Custom window
#' result <- add_EVWMA(mkt_data, n = 20)
#' # Example 3: Slim output
#' result <- add_EVWMA(mkt_data, n = 30, append = FALSE)
#' }
add_EVWMA <- function(mkt_data, n = 10, append = TRUE, output = c("tibble", "data.frame")) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close, volume.")
  }
  required_cols <- c("date", "code", "close", "volume")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    evwma_val <- EVWMA(sub$close, sub$volume, n = n)
    sub[["EVWMA"]] <- as.numeric(evwma_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original OHLCV columns ─────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "EVWMA"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Return in requested format ─────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
