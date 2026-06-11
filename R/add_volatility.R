#' @title Historical Volatility
#' @description
#' Computes historical volatility for each security in a long-format panel data
#' frame using one of several OHLCV-based estimators: close-to-close, Garman-
#' Klass, Parkinson, Rogers-Satchell, GK-YZ, or Yang-Zhang.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price/volume columns.
#' @param n Integer. Look-back window. Defaults to \code{10}.
#' @param calc Character. Volatility estimator. One of \code{"close"},
#'   \code{"garman.klass"}, \code{"parkinson"}, \code{"rogers.satchell"},
#'   \code{"gk.yz"}, or \code{"yang.zhang"}. Defaults to \code{"close"}.
#' @param N Integer. Number of trading periods per year used to annualize
#'   volatility. Defaults to \code{260}.
#' @param mean0 Logical. If \code{TRUE}, assume zero mean returns. Defaults to
#'   \code{FALSE}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the underlying \code{volatility}
#'   function.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{volatility.\{calc\}} containing annualized
#'   volatility values.
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date   = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code   = rep(c("AAPL", "MSFT"), each = 60),
#'   name   = rep(c("Apple", "Microsoft"), each = 60),
#'   open   = c(runif(60, 148, 198), runif(60, 298, 398)),
#'   high   = c(runif(60, 155, 205), runif(60, 305, 405)),
#'   low    = c(runif(60, 145, 195), runif(60, 295, 395)),
#'   close  = c(runif(60, 150, 200), runif(60, 300, 400)),
#'   volume = c(runif(60, 1e6, 2e6), runif(60, 5e5, 1.5e6))
#' )
#' # Example 1: Default close-to-close volatility
#' result <- add_volatility(mkt_data)
#' # Example 2: Garman-Klass estimator
#' result <- add_volatility(mkt_data, calc = "garman.klass")
#' # Example 3: Slim output with Yang-Zhang estimator
#' result <- add_volatility(mkt_data, calc = "yang.zhang", append = FALSE)
#' }
add_volatility <- function(mkt_data, n = 10, calc = "close", N = 260, mean0 = FALSE,
                           append = TRUE, output = c("tibble", "data.frame"), ...) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  calc <- match.arg(calc,
    c("close", "garman.klass", "parkinson", "rogers.satchell", "gk.yz", "yang.zhang"))

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, open, high, low, close.")
  }
  required_cols <- c("date", "code", "open", "high", "low", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  col_name <- paste0("volatility.", calc)

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    # Build full OHLC xts (5-column for yang.zhang compatibility)
    ohlc <- xts::xts(
      cbind(Open  = sub$open,
            High  = sub$high,
            Low   = sub$low,
            Close = sub$close),
      order.by = sub$date
    )

    # Compute volatility via volatility
    vol_val <- volatility(ohlc, n = n, calc = calc, N = N, mean0 = mean0, ...)
    sub[[col_name]] <- as.numeric(vol_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original columns ──────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", col_name), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
