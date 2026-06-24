#' @title Chaikin Volatility
#' @description
#' Computes Chaikin Volatility for each security in a long-format panel data
#' frame. The indicator measures the rate of change of the exponentially smoothed
#' high-low spread over \code{n} periods. A rising value indicates increasing
#' price volatility; a falling value indicates contraction. Developed by Marc
#' Chaikin.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, and \code{low}.
#' @param n Integer. Look-back window for the moving average and rate-of-change.
#'   Defaults to \code{10}.
#' @param maType Character. Moving average type applied to the high-low spread.
#'   Defaults to \code{"EMA"}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the \code{maType} function.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{chaikinVol_\{n\}} containing the Chaikin
#'   Volatility values expressed as a percentage rate of change.
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_chaikinVolatility(ettr_stocks)
add_chaikinVolatility <- function(mkt_data, n = 10, maType, append = TRUE,
                                  output = c("tibble", "data.frame"), ...) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  if (missing(maType)) maType <- "EMA"

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, high, low.")
  }
  required_cols <- c("date", "code", "high", "low")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  col_name <- paste0("chaikinVol_", n)

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    # Build HL xts
    hl <- xts::xts(
      cbind(High = sub$high, Low = sub$low),
      order.by = sub$date
    )

    # Moving average of the high-low spread
    spread <- hl[, "High"] - hl[, "Low"]
    mavg <- do.call(maType, c(list(spread), list(n = n, ...)))

    # Rate-of-change of the smoothed spread
    vol_val <- ROC(mavg, n, type = "discrete")

    sub[[col_name]] <- as.numeric(vol_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original columns ──────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", col_name), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
