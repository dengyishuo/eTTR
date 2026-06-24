#' @title Add Relative Strength Index (RSI)
#'
#' @description Computes the Relative Strength Index for each asset in a
#'   long-format panel data frame and appends the result as a new column named
#'   \code{RSI_<n>}. RSI measures the speed and magnitude of price changes on
#'   a 0-100 scale; readings above 70 are typically considered overbought and
#'   below 30 oversold.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param n Integer. Look-back window. Defaults to \code{14}.
#' @param maType Character or list. Moving average type used to smooth up and
#'   down moves. Defaults to \code{"EMA"} (with Wilder smoothing).
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the moving average function.
#'
#' @return The input data frame with an additional column \code{RSI_<n>}
#'   containing RSI values in the range \code{[0, 100]}.
#'
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#'
#' @examples
#' data(ettr_stocks)
#' result <- add_RSI(ettr_stocks)
add_RSI <- function(mkt_data, n = 14, maType, append = TRUE,
                    output = c("tibble", "data.frame"), ...) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  if (missing(maType)) maType <- "EMA"

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }
  required_cols <- c("date", "code", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  col_name <- paste0("RSI_", n)

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    close_xts <- xts::xts(sub$close, order.by = as.Date(sub$date))

    # Calculate price momentum
    up <- momentum(close_xts, n = 1, na.pad = TRUE)
    which.dn <- which(up < 0)
    dn <- up * 0
    dn[which.dn] <- -up[which.dn]
    up[which.dn] <- 0

    maArgs <- list(n = n, ...)
    ma_type <- maType
    if (identical(ma_type, "EMA") && is.null(maArgs$wilder)) {
      maArgs$wilder <- TRUE
    }

    if (is.list(ma_type)) {
      if (!all(sapply(ma_type, is.list)) || length(ma_type) != 2) {
        stop("If 'maType' is a list, you must specify *two* MAs")
      }
      for (i in 1:2) {
        if (!is.null(formals(ma_type[[i]][[1]])$n) && is.null(ma_type[[i]]$n)) {
          ma_type[[i]]$n <- n
        }
      }
      mavgUp <- do.call(ma_type[[1]][[1]], c(list(up), ma_type[[1]][-1]))
      mavgDn <- do.call(ma_type[[2]][[1]], c(list(dn), ma_type[[2]][-1]))
    } else {
      mavgUp <- do.call(ma_type, c(list(up), maArgs))
      mavgDn <- do.call(ma_type, c(list(dn), maArgs))
    }

    rsi <- 100 * mavgUp / (mavgUp + mavgDn)
    sub[[col_name]] <- as.numeric(rsi)
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
