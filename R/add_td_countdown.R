#' Add TD Countdown Indicator (Panel Data)
#'
#' Computes the TD Countdown phase of Tom DeMark's TD Sequential indicator,
#' which begins after TD Setup completes (count = 9). A bullish countdown
#' counts bars where \code{close <= prev_close} until reaching 13 (top
#' confirmation); a bearish countdown counts bars where
#' \code{close >= prev_close} until reaching 13 (bottom confirmation).
#'
#' \strong{Note}: This function requires that \code{add_td_setup()} has already
#' been called on \code{mkt_data}. The setup columns (\code{setup_bull_col} and
#' \code{setup_bear_col}) are used to detect when each setup phase completes.
#'
#' @param mkt_data A data.frame or tibble already containing TD Setup columns.
#'   Required columns: \code{date}, \code{code}, the close price column, and
#'   the two TD Setup columns.
#' @param close_col Character. Name of the close price column. Default \code{"close"}.
#' @param setup_bull_col Character. Name of the bullish setup count column.
#'   Default \code{"td_setup_bull"} (output of \code{add_td_setup()}).
#' @param setup_bear_col Character. Name of the bearish setup count column.
#'   Default \code{"td_setup_bear"}.
#' @param new_col Character. Prefix for output column names. Default
#'   \code{"td_countdown"}. Produces \code{<new_col>_bull} and
#'   \code{<new_col>_bear}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A tibble or data.frame with two new columns:
#' \describe{
#'   \item{\code{<new_col>_bull}}{Integer 1–13; current bullish countdown count.
#'     0 when no active countdown. 13 = top confirmation signal.}
#'   \item{\code{<new_col>_bear}}{Integer 1–13; current bearish countdown count.
#'     0 when no active countdown. 13 = bottom confirmation signal.}
#' }
#'
#' @examples
#' data(ettr_stocks)
#' df <- add_td_setup(ettr_stocks)
#' result <- add_td_countdown(df)
#' tail(result[
#'   result$td_countdown_bear > 0,
#'   c("date", "code", "td_setup_bear", "td_countdown_bear")
#' ], 10)
#'
#' @seealso \code{\link{add_td_setup}}
#' @export
add_td_countdown <- function(mkt_data,
                             close_col = "close",
                             setup_bull_col = "td_setup_bull",
                             setup_bear_col = "td_setup_bear",
                             new_col = "td_countdown",
                             append = TRUE,
                             output = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!is.data.frame(mkt_data)) {
    stop("'mkt_data' must be a data.frame or tibble.")
  }

  required <- c("date", "code", close_col, setup_bull_col, setup_bear_col)
  missing <- setdiff(required, colnames(mkt_data))
  if (length(missing) > 0) {
    stop(paste(
      "Missing columns (run add_td_setup() first):",
      paste(missing, collapse = ", ")
    ))
  }

  bull_cd_col <- paste0(new_col, "_bull")
  bear_cd_col <- paste0(new_col, "_bear")

  codes <- unique(mkt_data$code)
  res_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    n <- nrow(sub)
    cl <- sub[[close_col]]
    s_bull <- sub[[setup_bull_col]]
    s_bear <- sub[[setup_bear_col]]

    bull_cd <- integer(n)
    bear_cd <- integer(n)

    bull_active <- FALSE
    bear_active <- FALSE
    bull_count <- 0L
    bear_count <- 0L

    for (i in seq_len(n)) {
      # Detect setup completion (9)
      if (!is.na(s_bull[i]) && s_bull[i] == 9L) {
        bull_active <- TRUE
        bull_count <- 0L
      }
      if (!is.na(s_bear[i]) && s_bear[i] == 9L) {
        bear_active <- TRUE
        bear_count <- 0L
      }

      if (i > 1 && !is.na(cl[i]) && !is.na(cl[i - 1])) {
        # Bullish countdown: close <= prev close
        if (bull_active && bull_count < 13L) {
          if (cl[i] <= cl[i - 1]) {
            bull_count <- bull_count + 1L
          }
          bull_cd[i] <- bull_count
          if (bull_count == 13L) bull_active <- FALSE
        }
        # Bearish countdown: close >= prev close
        if (bear_active && bear_count < 13L) {
          if (cl[i] >= cl[i - 1]) {
            bear_count <- bear_count + 1L
          }
          bear_cd[i] <- bear_count
          if (bear_count == 13L) bear_active <- FALSE
        }
      }
    }

    sub[[bull_cd_col]] <- bull_cd
    sub[[bear_cd_col]] <- bear_cd
    sub
  })

  res <- do.call(rbind, res_list)
  res <- res[order(res$date, res$code), ]

  if (!append) {
    keep <- intersect(c("date", "code", "name", bull_cd_col, bear_cd_col), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
