#' @title ZigZag Indicator
#' @description
#' Computes the ZigZag indicator for each security in a long-format panel data
#' frame. ZigZag filters price noise by identifying significant swing highs and
#' lows that exceed a minimum change threshold, then linearly interpolates
#' between those extremes. The result is useful for identifying chart patterns,
#' Elliott Wave structures, and support/resistance levels. Required columns:
#' \code{high}, \code{low}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, and \code{low}.
#' @param change Numeric. Minimum price change required to qualify as a new
#'   swing extreme. When \code{percent = TRUE} this is a percentage value;
#'   otherwise it is an absolute price amount. Defaults to \code{10}.
#' @param percent Logical. If \code{TRUE} (default), \code{change} is
#'   interpreted as a percentage change between successive swing extremes.
#' @param retrace Logical. If \code{TRUE}, \code{change} defines the minimum
#'   retracement percentage from the last extreme rather than the total move.
#'   Defaults to \code{FALSE}.
#' @param lastExtreme Logical. If \code{TRUE} (default), use the most extreme
#'   price when multiple consecutive bars qualify as the same extreme direction.
#'   Defaults to \code{TRUE}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{ZigZag} containing the linearly interpolated
#'   ZigZag line values. Rows between identified swing extremes are filled by
#'   linear interpolation; leading rows with insufficient data are \code{NA}.
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_ZigZag(ettr_stocks)
add_ZigZag <- function(mkt_data, change = 10, percent = TRUE, retrace = FALSE,
                       lastExtreme = TRUE, append = TRUE,
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

    hl_xts <- xts::xts(
      cbind(High = sub$high, Low = sub$low),
      order.by = as.Date(sub$date)
    )
    hl_na <- naCheck(hl_xts, 0)

    high <- hl_xts[hl_na$nonNA, 1]
    low  <- hl_xts[hl_na$nonNA, 2]

    zz <- .Call(
      ettr_zigzag,
      as.numeric(high),
      as.numeric(low),
      as.numeric(change),
      as.logical(percent),
      as.logical(retrace),
      as.logical(lastExtreme)
    )

    zz <- na.approx(zz, na.rm = FALSE)
    zz <- c(rep(NA, hl_na$NAs), zz)

    sub[["ZigZag"]] <- as.numeric(zz)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Column selection ───────────────────────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "ZigZag"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
