#' @title Add Weighted Moving Average (WMA)
#'
#' @description Computes a Weighted Moving Average for each asset in a
#'   long-format panel data frame and appends the result as a new column named
#'   \code{WMA_<n>}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param n Integer. Look-back window. Defaults to \code{10}.
#' @param wts Numeric vector of length \code{n}. Weights applied to each
#'   observation in the window. Defaults to \code{1:n} (linearly increasing).
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return The input data frame with an additional column \code{WMA_<n>}
#'   containing the weighted moving average of \code{close}.
#'
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#'
#' @examples
#' data(ettr_stocks)
#' result <- add_WMA(ettr_stocks)
add_WMA <- function(mkt_data, n = 10, wts = 1:n, append = TRUE,
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

  # ── Split-apply-combine over each asset ───────────────────────────────────
  col_name <- paste0("WMA_", n)
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
    ma <- WMA(close_xts, n = n, wts = wts)
    sub[[col_name]] <- as.numeric(ma)
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
