#' @title On-Balance Volume (OBV)
#' @description
#' Computes the On-Balance Volume for each security in a long-format panel data
#' frame. OBV is a cumulative volume indicator that adds volume on up-close days
#' and subtracts it on down-close days, serving as a proxy for buying and selling
#' pressure.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price/volume columns.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{OBV} containing the cumulative On-Balance
#'   Volume values.
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
#' result <- add_OBV(mkt_data)
#' # Example 2: Slim output
#' result <- add_OBV(mkt_data, append = FALSE)
#' # Example 3: Return as data.frame
#' result <- add_OBV(mkt_data, output = "data.frame")
#' }
# eTTR: Enhanced Technical Trading Rules
add_OBV <- function(mkt_data, append = TRUE, output = c("tibble", "data.frame")) {

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

    obv_val <- OBV(sub$close, sub$volume)
    sub[["OBV"]] <- as.numeric(obv_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original OHLCV columns ─────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "OBV"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Return in requested format ─────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
