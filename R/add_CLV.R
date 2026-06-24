#' @title Close Location Value (CLV)
#' @description
#' Computes the Close Location Value for each security in a long-format panel
#' data frame. CLV relates the closing price to the day's trading range,
#' returning values in \code{[-1, 1]}. A value of \code{+1} means the close
#' is at the high; \code{-1} means it is at the low; \code{0} means it is
#' exactly at the midpoint. CLV is used as a building block for the Chaikin
#' Accumulation/Distribution line.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, \code{low}, and
#'   \code{close}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{CLV} containing values in \code{[-1, 1]}.
#' @export
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_CLV(ettr_stocks)
add_CLV <- function(mkt_data, append = TRUE, output = c("tibble", "data.frame")) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, high, low, close.")
  }
  required_cols <- c("date", "code", "high", "low", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # Split-apply-combine
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    h <- sub$high
    l <- sub$low
    cl <- sub$close
    clv_val <- ((cl - l) - (h - cl)) / (h - l)
    clv_val[is.nan(clv_val) | is.infinite(clv_val)] <- 0
    sub[["CLV"]] <- clv_val
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original OHLCV columns ─────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "CLV"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Return in requested format ─────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
