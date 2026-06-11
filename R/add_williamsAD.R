#' @title Williams Accumulation/Distribution
#' @description
#' Computes the Williams Accumulation/Distribution line for each security in a
#' long-format panel data frame. The indicator accumulates buying or selling
#' pressure based on whether price closes above or below the prior close,
#' measuring the distance from the true low or true high respectively.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price/volume columns.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{williamsAD} containing the cumulative
#'   Williams Accumulation/Distribution values.
#' @export
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date   = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code   = rep(c("AAPL", "MSFT"), each = 60),
#'   name   = rep(c("Apple", "Microsoft"), each = 60),
#'   high   = c(runif(60, 155, 205), runif(60, 305, 405)),
#'   low    = c(runif(60, 145, 195), runif(60, 295, 395)),
#'   close  = c(runif(60, 150, 200), runif(60, 300, 400)),
#'   volume = c(runif(60, 1e6, 2e6), runif(60, 5e5, 1.5e6))
#' )
#' # Example 1: Default parameters
#' result <- add_williamsAD(mkt_data)
#' # Example 2: Slim output
#' result <- add_williamsAD(mkt_data, append = FALSE)
#' # Example 3: Return as data.frame
#' result <- add_williamsAD(mkt_data, output = "data.frame")
#' }
add_williamsAD <- function(mkt_data, append = TRUE, output = c("tibble", "data.frame")) {

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

    h  <- sub$high
    l  <- sub$low
    cl <- sub$close

    prior_close <- c(NA, cl[-length(cl)])
    true_low    <- pmin(l, prior_close)
    true_high   <- pmax(h, prior_close)

    d_cl <- cl - prior_close  # change in close

    ad_raw <- cl - ifelse(d_cl > 0, true_low, true_high)
    ad_raw[!is.na(d_cl) & d_cl == 0] <- 0
    ad_raw[is.na(d_cl)] <- NA

    non_na <- !is.na(ad_raw)
    ad_val <- rep(NA_real_, length(ad_raw))
    ad_val[non_na] <- cumsum(ad_raw[non_na])

    sub[["williamsAD"]] <- ad_val
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original OHLCV columns ─────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "williamsAD"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Return in requested format ─────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
