#' @title Keltner Channels
#' @description
#' Computes Keltner Channels for each security in a long-format panel data frame.
#' The channel is formed by a moving average of the typical price (center line)
#' plus and minus a multiple of the Average True Range.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, \code{low}, and
#'   \code{close}.
#' @param n Integer. Look-back window for both the moving average and the ATR.
#'   Defaults to \code{20}.
#' @param maType Moving average type applied to the typical price (function name
#'   as a string or function object). Defaults to \code{"EMA"}.
#' @param atr Numeric. Multiplier applied to the ATR to set the channel width.
#'   Defaults to \code{2}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the \code{maType} function.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with columns:
#'   \describe{
#'     \item{kc_dn_\{n\}}{Lower Keltner Channel band (center minus atr * ATR).}
#'     \item{kc_mavg_\{n\}}{Center moving average of the typical price.}
#'     \item{kc_up_\{n\}}{Upper Keltner Channel band (center plus atr * ATR).}
#'   }
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_keltnerChannels(ettr_stocks)
add_keltnerChannels <- function(mkt_data, n = 20, maType, atr = 2, append = TRUE,
                                output = c("tibble", "data.frame"), ...) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  if (missing(maType)) maType <- "EMA"

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

    # Typical price as the MA input
    tp <- xts::xts(rowMeans(hlc), order.by = sub$date)

    mavg_val <- do.call(maType, c(list(tp), list(n = n, ...)))
    atr_val <- TTR::ATR(hlc, n = n)

    up_val <- mavg_val + atr * atr_val[, "atr"]
    dn_val <- mavg_val - atr * atr_val[, "atr"]

    sub[[paste0("kc_dn_", n)]] <- as.numeric(dn_val)
    sub[[paste0("kc_mavg_", n)]] <- as.numeric(mavg_val)
    sub[[paste0("kc_up_", n)]] <- as.numeric(up_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original columns ──────────────────────────────────────
  if (!append) {
    new_cols <- c(paste0("kc_dn_", n), paste0("kc_mavg_", n), paste0("kc_up_", n))
    keep <- intersect(c("date", "code", "name", new_cols), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
