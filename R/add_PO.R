#' @title Add Price Oscillator (PO)
#'
#' @description Computes the Price Oscillator for each asset in a long-format
#'   panel data frame and appends the result as a new column named \code{PO}.
#'   The PO is the difference (or percentage ratio) between a short-period and
#'   a long-period moving average of \code{close}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param n_short Integer. Short-period window. Must be positive and less than
#'   \code{n_long}. Defaults to \code{12}.
#' @param n_long Integer. Long-period window. Must be greater than
#'   \code{n_short}. Defaults to \code{26}.
#' @param type Character. \code{"difference"} (default) returns the absolute
#'   difference between the two MAs; \code{"percent"} returns the percentage
#'   difference relative to the long-period MA.
#' @param ma_type Character. Moving average type: \code{"SMA"} (default) or
#'   \code{"EMA"}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return The input data frame with an additional column \code{PO} containing
#'   the Price Oscillator values.
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
#' result <- add_PO(mkt_data)
#' # Example 2: Percentage type with EMA
#' result <- add_PO(mkt_data, type = "percent", ma_type = "EMA")
#' # Example 3: Slim output
#' result <- add_PO(mkt_data, n_short = 5, n_long = 20, append = FALSE)
#' }
add_PO <- function(mkt_data, n_short = 12, n_long = 26,
                   type = c("difference", "percent"),
                   ma_type = c("SMA", "EMA"),
                   append = TRUE,
                   output = c("tibble", "data.frame")) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output  <- match.arg(output)
  type    <- match.arg(type)
  ma_type <- match.arg(ma_type)

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }
  required_cols <- c("date", "code", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }
  if (!is.numeric(n_short) || !is.numeric(n_long) || n_short <= 0 || n_long <= 0 || n_short >= n_long) {
    stop("n_short must be a positive integer smaller than n_long")
  }

  # ── Split-apply-combine ────────────────────────────────────────────────────
  calculate_ma <- function(p, n) {
    if (ma_type == "SMA") SMA(p, n = n) else EMA(p, n = n)
  }

  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    close_xts <- xts::xts(sub$close, order.by = as.Date(sub$date))

    ma_short <- calculate_ma(close_xts, n_short)
    ma_long  <- calculate_ma(close_xts, n_long)

    if (type == "difference") {
      po_val <- ma_short - ma_long
    } else {
      po_val <- ((ma_short - ma_long) / ma_long) * 100
    }

    sub[["PO"]] <- as.numeric(po_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Column selection ───────────────────────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "PO"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
