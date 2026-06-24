#' @title Price Bands (Chandelier)
#' @description
#' Computes Price Bands (also known as Chandelier Bands) for each security in a
#' long-format panel data frame. The bands are formed around a slow moving
#' average. Band width is \code{sd} multiples of the rolling standard deviation
#' of the difference between the slow and fast moving averages. When
#' \code{centered = TRUE} the center line shifts to the normalized deviation
#' from the slow average, optionally using a doubled window (\code{lavg}).
#' Required column: \code{close}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and \code{close}.
#' @param n Integer. Look-back window for the slow moving average and rolling
#'   standard deviation. Defaults to \code{20}.
#' @param maType Character. Name of the moving average function to use (e.g.
#'   \code{"SMA"}, \code{"EMA"}). Defaults to \code{"SMA"}.
#' @param sd Numeric. Number of standard deviations used to set band width.
#'   Defaults to \code{2}.
#' @param fastn Integer. Look-back window for the fast moving average used to
#'   compute the deviation. Defaults to \code{2}.
#' @param centered Logical. If \code{TRUE}, shift the center line based on a
#'   normalized slow-minus-fast deviation. Defaults to \code{FALSE}.
#' @param lavg Logical. Applies only when \code{centered = TRUE}. If
#'   \code{TRUE}, double \code{n} when computing the centering average.
#'   Defaults to \code{FALSE}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the \code{maType} function.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with columns:
#'   \describe{
#'     \item{PBands_dn}{Lower price band (center minus \code{sd} standard
#'       deviations).}
#'     \item{PBands_center}{Center band: the slow moving average (or the
#'       adjusted center when \code{centered = TRUE}).}
#'     \item{PBands_up}{Upper price band (center plus \code{sd} standard
#'       deviations).}
#'   }
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_PBands(ettr_stocks)
add_PBands <- function(mkt_data, n = 20, maType = "SMA", sd = 2, fastn = 2,
                       centered = FALSE, lavg = FALSE, append = TRUE,
                       output = c("tibble", "data.frame"), ...) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  if (missing(maType)) maType <- "SMA"

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }
  required_cols <- c("date", "code", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    prices <- xts::xts(sub$close, order.by = as.Date(sub$date))

    maArgs     <- list(n = n, ...)
    maFastArgs <- list(n = fastn, ...)

    mavg     <- do.call(maType, c(list(prices), maArgs))
    fastmavg <- do.call(maType, c(list(prices), maFastArgs))
    sdev     <- TTR::runSD((mavg - fastmavg), n = n, sample = FALSE)

    if (!isTRUE(centered)) {
      center <- mavg
    } else {
      centerrun <- (mavg - fastmavg) / sdev
      if (isTRUE(lavg)) maArgs <- list(n = (n * 2), ...)
      center <- mavg + (do.call(maType, c(list(centerrun), maArgs)))
    }

    up <- center + sd * sdev
    dn <- center - sd * sdev

    sub[["PBands_dn"]]     <- as.numeric(dn)
    sub[["PBands_center"]] <- as.numeric(center)
    sub[["PBands_up"]]     <- as.numeric(up)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Column selection ───────────────────────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", "PBands_dn", "PBands_center", "PBands_up"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
