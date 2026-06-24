#' @title Add Double Exponential Moving Average (DEMA)
#'
#' @description Computes a Double Exponential Moving Average for each asset in
#'   a long-format panel data frame and appends the result as a new column
#'   named \code{DEMA_<n>}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param n Integer. Look-back window. Defaults to \code{10}.
#' @param v Numeric. Volume factor in the range \code{[0, 1]}. When \code{v = 1}
#'   (default) the result is a standard DEMA; values between 0 and 1 produce a
#'   blended MA.
#' @param wilder Logical. If \code{TRUE}, use Wilder's EMA smoothing ratio.
#'   Defaults to \code{FALSE}.
#' @param ratio Numeric. Custom smoothing ratio. Defaults to \code{NULL}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return The input data frame with an additional column \code{DEMA_<n>}
#'   containing the double exponential moving average of \code{close}.
#'
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#'
#' @examples
#' data(ettr_stocks)
#' result <- add_DEMA(ettr_stocks)
add_DEMA <- function(mkt_data, n = 10, v = 1, wilder = FALSE, ratio = NULL, append = TRUE,
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
  if (v < 0 || v > 1) stop("Please ensure 0 <= v <= 1")

  # ── Split-apply-combine over each asset ───────────────────────────────────
  col_name <- paste0("DEMA_", n)
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
    dema <- DEMA(close_xts, n = n, v = v, wilder = wilder, ratio = ratio)
    sub[[col_name]] <- as.numeric(dema)
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
