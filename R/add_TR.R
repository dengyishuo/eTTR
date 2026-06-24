#' @title True Range
#' @description
#' Computes the True Range for each security in a long-format panel data frame.
#' True Range is the greatest of: current high minus current low, absolute value
#' of current high minus previous close, and absolute value of current low minus
#' previous close.
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
#'   \code{code}, with columns:
#'   \describe{
#'     \item{tr}{True range.}
#'     \item{trueHigh}{True high used in the TR calculation.}
#'     \item{trueLow}{True low used in the TR calculation.}
#'   }
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_TR(ettr_stocks)
add_TR <- function(mkt_data, append = TRUE, output = c("tibble", "data.frame")) {

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

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    # Build HLC xts
    hlc <- xts::xts(
      cbind(High = sub$high, Low = sub$low, Close = sub$close),
      order.by = sub$date
    )

    tr_val <- TR(hlc)
    sub[["tr"]]       <- as.numeric(tr_val[, "tr"])
    sub[["trueHigh"]] <- as.numeric(tr_val[, "trueHigh"])
    sub[["trueLow"]]  <- as.numeric(tr_val[, "trueLow"])
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original columns ──────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "tr", "trueHigh", "trueLow"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
