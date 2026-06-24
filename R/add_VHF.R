#' @title Vertical Horizontal Filter (VHF)
#' @description
#' Computes the Vertical Horizontal Filter for each security in a long-format
#' panel data frame. VHF measures the degree of trend versus consolidation by
#' comparing the highest high minus lowest low to the sum of absolute price
#' changes over a rolling window.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, \code{low}, and
#'   \code{close}.
#' @param n Integer. Look-back window for rolling high, low, and price change
#'   sum. Defaults to \code{28}.
#' @param na.rm Logical. If \code{TRUE} (default), replace \code{NA} values in
#'   the output with the value of \code{zero.replace}.
#' @param inf.replace Replacement value for infinite VHF results. Defaults to
#'   \code{NA}.
#' @param zero.replace Replacement value for VHF results where the cumulative
#'   price change denominator is near zero. Defaults to \code{NA}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column:
#'   \describe{
#'     \item{VHF_\{n\}}{Vertical Horizontal Filter values scaled by 100.
#'       Higher values indicate a trending market; lower values indicate
#'       consolidation.}
#'   }
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_VHF(ettr_stocks)
add_VHF <- function(mkt_data, n = 28, na.rm = TRUE, inf.replace = NA,
                    zero.replace = NA, append = TRUE,
                    output = c("tibble", "data.frame")) {

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
  if (n <= 0 || !is.numeric(n)) stop("n must be a positive number")

  col_name <- paste0("VHF_", n)

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    high  <- xts::xts(sub$high,  order.by = as.Date(sub$date))
    low   <- xts::xts(sub$low,   order.by = as.Date(sub$date))
    close <- xts::xts(sub$close, order.by = as.Date(sub$date))

    hmax         <- TTR::runMax(high, n)
    lmin         <- TTR::runMin(low,  n)
    price_range  <- hmax - lmin
    deltas       <- diff(close)
    price_change <- TTR::runSum(abs(deltas), n)

    vhf <- price_range / pmax(price_change, 1e-10) * 100

    # Apply NA/INF replacements
    if (na.rm) vhf[is.na(vhf)] <- zero.replace
    if (!is.na(inf.replace))   vhf[is.infinite(vhf)] <- inf.replace
    if (!is.na(zero.replace)) {
      zero_idx <- which(price_change < 1e-10)
      if (length(zero_idx) > 0) vhf[zero_idx] <- zero.replace
    }

    sub[[col_name]] <- as.numeric(vhf)
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
