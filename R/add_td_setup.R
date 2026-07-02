#' Add TD Setup Indicator (Panel Data)
#'
#' Computes the TD Setup phase of Tom DeMark's TD Sequential indicator for each
#' security in a long-format panel. A bullish setup counts consecutive bars where
#' \code{close > close[4 bars ago]}; a bearish setup counts consecutive bars where
#' \code{close < close[4 bars ago]}. Each count resets to zero when the condition
#' breaks. A value of 9 signals completion (trend exhaustion warning).
#'
#' @param mkt_data A data.frame or tibble in long format. Required columns:
#'   \code{date}, \code{code}, and the close price column.
#' @param close_col Character. Name of the close price column. Default \code{"close"}.
#' @param new_col Character. Prefix for output column names. Default \code{"td_setup"}.
#'   Produces \code{<new_col>_bull} and \code{<new_col>_bear}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}; if \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name} (if present), and the new columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A tibble or data.frame with two new columns:
#' \describe{
#'   \item{\code{<new_col>_bull}}{Integer 1–9; current bullish setup count.
#'     0 when the bullish condition is not met. 9 = bullish setup complete.}
#'   \item{\code{<new_col>_bear}}{Integer 1–9; current bearish setup count.
#'     0 when the bearish condition is not met. 9 = bearish setup complete.}
#' }
#'
#' @examples
#' data(ettr_stocks)
#' result <- add_td_setup(ettr_stocks)
#' head(result[!is.na(result$td_setup_bull), c("date", "code", "td_setup_bull", "td_setup_bear")])
#'
#' @seealso \code{\link{add_td_countdown}}
#' @export
add_td_setup <- function(mkt_data,
                         close_col = "close",
                         new_col = "td_setup",
                         append = TRUE,
                         output = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!is.data.frame(mkt_data)) {
    stop("'mkt_data' must be a data.frame or tibble.")
  }

  required <- c("date", "code", close_col)
  missing <- setdiff(required, colnames(mkt_data))
  if (length(missing) > 0) {
    stop(paste("Missing columns:", paste(missing, collapse = ", ")))
  }

  bull_col <- paste0(new_col, "_bull")
  bear_col <- paste0(new_col, "_bear")

  codes <- unique(mkt_data$code)
  res_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    n <- nrow(sub)
    cl <- sub[[close_col]]

    bull <- integer(n)
    bear <- integer(n)

    for (i in seq_len(n)) {
      if (i <= 4) next
      ref <- cl[i - 4]
      cur <- cl[i]

      if (!is.na(cur) && !is.na(ref)) {
        if (cur > ref) {
          prev_bull <- if (i > 1) bull[i - 1] else 0L
          bull[i] <- min(prev_bull + 1L, 9L)
          bear[i] <- 0L
        } else if (cur < ref) {
          prev_bear <- if (i > 1) bear[i - 1] else 0L
          bear[i] <- min(prev_bear + 1L, 9L)
          bull[i] <- 0L
        } else {
          bull[i] <- 0L
          bear[i] <- 0L
        }
      }
    }

    sub[[bull_col]] <- bull
    sub[[bear_col]] <- bear
    sub
  })

  res <- do.call(rbind, res_list)
  res <- res[order(res$date, res$code), ]

  if (!append) {
    keep <- intersect(c("date", "code", "name", bull_col, bear_col), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
