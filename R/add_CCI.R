#' @title Commodity Channel Index (CCI)
#' @description
#' Computes the Commodity Channel Index for each security in a long-format panel
#' data frame. CCI measures how far the current typical price (mean of high,
#' low, close) deviates from its \code{n}-period moving average, scaled by the
#' mean absolute deviation. Values above \code{+100} suggest overbought
#' conditions; values below \code{-100} suggest oversold conditions. Required
#' columns: \code{high}, \code{low}, \code{close}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, \code{low}, and
#'   \code{close}.
#' @param n Integer. Look-back window. Defaults to \code{20}.
#' @param maType Character. Name of the moving average function to use (e.g.
#'   \code{"SMA"}, \code{"EMA"}). Defaults to \code{"SMA"}.
#' @param c Numeric. Scaling constant applied to the mean deviation.
#'   Defaults to \code{0.015}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the \code{maType} function.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{CCI_<n>} containing the Commodity Channel
#'   Index values. Typical readings fall between -200 and +200, with extreme
#'   values indicating overbought or oversold conditions.
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @importFrom TTR CCI
#' @examples
#' data(ettr_stocks)
#' result <- add_CCI(ettr_stocks)
add_CCI <- function(mkt_data, n = 20, maType, c = 0.015, append = TRUE,
                    output = c("tibble", "data.frame"), ...) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  if (missing(maType)) maType <- "SMA"

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, high, low, close.")
  }
  required_cols <- c("date", "code", "high", "low", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  col_name <- paste0("CCI_", n)

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    # Build HLC xts and compute typical price
    hlc_xts <- xts::xts(
      cbind(High = sub$high, Low = sub$low, Close = sub$close),
      order.by = as.Date(sub$date)
    )
    typical <- xts::xts(rowMeans(hlc_xts), order.by = as.Date(sub$date))

    ma_args <- list(n = n, ...)
    mavg <- do.call(maType, c(list(typical), ma_args))
    mean_dev <- TTR::runMAD(typical, n, center = mavg, stat = "mean")
    cci_val <- (typical - mavg) / (c * mean_dev)

    sub[[col_name]] <- as.numeric(cci_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Column selection ───────────────────────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", col_name), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
