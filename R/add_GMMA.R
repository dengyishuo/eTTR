#' @title Add Guppy Multiple Moving Average (GMMA)
#'
#' @description Computes the Guppy Multiple Moving Average for each asset in a
#'   long-format panel data frame. Two groups of exponential moving averages -
#'   a short-term group for traders and a long-term group for investors - are
#'   computed and appended as columns named \code{short_lag_<p>} and
#'   \code{long_lag_<p>} respectively.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param short Integer vector. Periods for the short-term group.
#'   Defaults to \code{c(3, 5, 8, 10, 12, 15)}.
#' @param long Integer vector. Periods for the long-term group.
#'   Defaults to \code{c(30, 35, 40, 45, 50, 60)}.
#' @param maType Character or function name. The moving average type to apply.
#'   Defaults to \code{"EMA"}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return The input data frame with additional columns \code{short_lag_<p>}
#'   (one per short period) and \code{long_lag_<p>} (one per long period).
#'
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#'
#' @examples
#' data(ettr_stocks)
#' result <- add_GMMA(ettr_stocks)
add_GMMA <- function(mkt_data,
                     short = c(3, 5, 8, 10, 12, 15),
                     long = c(30, 35, 40, 45, 50, 60),
                     maType, append = TRUE,
                     output = c("tibble", "data.frame")) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  if (missing(maType)) maType <- "EMA"

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }
  required_cols <- c("date", "code", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # Build output column names once
  short_names <- paste0("short_lag_", short)
  long_names <- paste0("long_lag_", long)
  all_names <- c(short_names, long_names)

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    x <- xts::xts(sub$close, order.by = as.Date(sub$date))

    fn <- function(g) as.numeric(do.call(maType, list(x, n = g)))
    gmma_mat <- do.call(cbind, lapply(c(short, long), fn))
    colnames(gmma_mat) <- all_names

    for (nm in all_names) sub[[nm]] <- gmma_mat[, nm]
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Column selection ───────────────────────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", all_names), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
