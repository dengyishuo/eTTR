#' @title DVI Indicator
#' @description
#' Computes the DVI indicator for each security in a long-format panel data
#' frame. DVI combines a magnitude component (smoothed price returns) and a
#' stretch component (up/down day counts) using percent-rank normalization,
#' producing a composite oscillator bounded between 0 and 1. Values above 0.5
#' are generally interpreted as bullish; values below 0.5 as bearish. Required
#' column: \code{close}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and \code{close}.
#' @param n Integer. Look-back window for percent-rank normalization. Defaults
#'   to \code{252}.
#' @param wts Numeric vector of length 2. Weights for the magnitude and stretch
#'   components respectively. The vector is normalized to sum to 1 internally.
#'   Defaults to \code{c(0.8, 0.2)}.
#' @param smooth Integer. Smoothing period applied to price returns before
#'   computing the magnitude component. Defaults to \code{3}.
#' @param magnitude Integer vector of length 3 specifying the short, long, and
#'   smoothing periods for the magnitude component. Defaults to
#'   \code{c(5, 100, 5)}.
#' @param stretch Integer vector of length 3 specifying the short, long, and
#'   smoothing periods for the stretch component. Defaults to
#'   \code{c(10, 100, 2)}.
#' @param exact.multiplier Numeric. Multiplier used in the percent-rank
#'   calculation. Defaults to \code{1}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with columns:
#'   \describe{
#'     \item{dvi.mag}{Percent-ranked magnitude component of DVI (0 to 1).}
#'     \item{dvi.str}{Percent-ranked stretch component of DVI (0 to 1).}
#'     \item{dvi}{Composite DVI value: weighted sum of \code{dvi.mag} and
#'       \code{dvi.str}, bounded in (0, 1).}
#'   }
#' @export
#' @importFrom xts xts
#' @importFrom xts lag.xts
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_DVI(ettr_stocks)
add_DVI <- function(mkt_data, n = 252, wts = c(0.8, 0.2), smooth = 3,
                    magnitude = c(5, 100, 5), stretch = c(10, 100, 2),
                    exact.multiplier = 1, append = TRUE,
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

  # Normalize weights
  wts <- wts / sum(wts)

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    price <- xts::xts(sub$close, order.by = as.Date(sub$date))

    # Magnitude component: smoothed price returns
    r <- price / SMA(price, smooth) - 1
    mag <- SMA((SMA(r, magnitude[1]) + SMA(r, magnitude[2]) / 10) / 2, magnitude[3])

    # Stretch component: up/down day counts
    b <- ifelse(price > lag.xts(price), 1, -1)
    str <- SMA((TTR::runSum(b, stretch[1]) + TTR::runSum(b, stretch[2]) / 10) / 2, stretch[3])

    # Percent rank
    dvi_mag <- TTR::runPercentRank(mag, n, FALSE, exact.multiplier)
    dvi_str <- TTR::runPercentRank(str, n, FALSE, exact.multiplier)
    dvi_val <- wts[1] * dvi_mag + wts[2] * dvi_str

    sub[["dvi.mag"]] <- as.numeric(dvi_mag)
    sub[["dvi.str"]] <- as.numeric(dvi_str)
    sub[["dvi"]] <- as.numeric(dvi_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Column selection ───────────────────────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "dvi.mag", "dvi.str", "dvi"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
