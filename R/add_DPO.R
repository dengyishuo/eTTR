#' @title Add Detrended Price Oscillator (DPO)
#'
#' @description Computes the Detrended Price Oscillator for each asset in a
#'   long-format panel data frame and appends the result as a new column named
#'   \code{DPO_<n>}. The DPO removes the trend from price by subtracting a
#'   shifted moving average, isolating shorter-term price cycles.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param n Integer. Look-back window for the moving average. Defaults to
#'   \code{10}.
#' @param maType Character or function. Moving average type. Defaults to
#'   \code{"SMA"}.
#' @param shift Integer. Number of periods to shift the MA back in time.
#'   Defaults to \code{n / 2 + 1}.
#' @param percent Logical. If \code{FALSE} (default), return the absolute
#'   difference. If \code{TRUE}, return the percentage difference relative to
#'   the shifted MA.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the moving average function.
#'
#' @return The input data frame with an additional column \code{DPO_<n>}
#'   containing the detrended price oscillator values.
#'
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date  = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code  = rep(c("AAPL", "MSFT"), each = 60),
#'   name  = rep(c("Apple", "Microsoft"), each = 60),
#'   close = c(runif(60, 150, 200), runif(60, 300, 400))
#' )
#' # Example 1: Default parameters
#' result <- add_DPO(mkt_data)
#' # Example 2: Custom window with EMA
#' result <- add_DPO(mkt_data, n = 20, maType = "EMA")
#' # Example 3: Slim output as percentage
#' result <- add_DPO(mkt_data, n = 14, percent = TRUE, append = FALSE)
#' }
add_DPO <- function(mkt_data, n = 10, maType, shift = n / 2 + 1, percent = FALSE,
                    append = TRUE, output = c("tibble", "data.frame"), ...) {

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

  # ── Split-apply-combine over each asset ───────────────────────────────────
  col_name <- paste0("DPO_", n)
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    if (n > nrow(sub)) {
      warning(sprintf("Skipping code '%s': n = %d exceeds available rows (%d).", cd, n, nrow(sub)))
      sub[[col_name]] <- NA_real_
      return(sub)
    }

    close_xts <- xts::xts(sub$close, order.by = sub$date)

    ma_args <- list(n = n, ...)
    mavg <- do.call(maType, c(list(close_xts), ma_args))
    mavg <- xts::lag.xts(mavg, -shift)

    if (percent) {
      dpo <- 100 * (close_xts / mavg - 1)
    } else {
      dpo <- close_xts - mavg
    }

    sub[[col_name]] <- as.numeric(dpo)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Slim output when append = FALSE ───────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", col_name), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format conversion ───────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
