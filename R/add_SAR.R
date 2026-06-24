#' @title Parabolic SAR
#' @description
#' Computes the Parabolic Stop-and-Reverse (SAR) indicator for each security in
#' a long-format panel data frame. The SAR is a trend-following indicator that
#' provides potential entry and exit points. Developed by J. Welles Wilder.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, and \code{low}.
#' @param accel Numeric vector of length 2. Acceleration factor: the first
#'   element is the initial step and the second is the maximum step. Defaults
#'   to \code{c(0.02, 0.2)}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column:
#'   \describe{
#'     \item{sar}{Parabolic SAR value for each row.}
#'   }
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_SAR(ettr_stocks)
add_SAR <- function(mkt_data, accel = c(.02, .2), append = TRUE,
                    output = c("tibble", "data.frame")) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, high, low.")
  }
  required_cols <- c("date", "code", "high", "low")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

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

    sar_val <- SAR(hl, accel = accel)
    sub[["sar"]] <- as.numeric(sar_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original columns ──────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "sar"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
