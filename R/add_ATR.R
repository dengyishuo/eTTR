#' @title Calculate Average True Range (ATR)
#' @description
#' Computes the Average True Range (ATR) for each security in a long-format panel
#' data frame. ATR measures market volatility by applying a moving average (default
#' Wilder's EMA) to the true range.
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, \code{low}, and
#'   \code{close}. Additional columns (e.g. \code{name}, \code{open},
#'   \code{volume}) are preserved.
#' @param n Integer. Number of periods for the moving average. Defaults to
#'   \code{14}.
#' @param maType Moving average type passed to TTR internals. Defaults to
#'   \code{"EMA"} (Wilder's EMA).
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the moving average function.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with columns:
#'   \describe{
#'     \item{tr_\{n\}}{True range.}
#'     \item{atr_\{n\}}{Average true range over \code{n} periods.}
#'     \item{trueHigh_\{n\}}{True high used in TR calculation.}
#'     \item{trueLow_\{n\}}{True low used in TR calculation.}
#'   }
#' @details
#' ATR was introduced by J. Welles Wilder. It accounts for the full price range
#' of each period, including gaps between sessions. The default Wilder EMA uses
#' a smoothing factor of 1/n.
#' @references
#' Wilder, J. Welles. "New Concepts in Technical Trading Systems." 1978.
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @importFrom TTR ATR
#' @examples
#' \dontrun{
#' # Build a minimal mkt_data panel
#' mkt_data <- data.frame(
#'   date = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code = rep(c("AAA", "BBB"), each = 60),
#'   open = runif(120, 10, 20),
#'   high = runif(120, 20, 30),
#'   low = runif(120, 5, 15),
#'   close = runif(120, 10, 25),
#'   volume = runif(120, 1e6, 2e6)
#' )
#'
#' # Default: append = TRUE, output = "tibble"
#' result1 <- add_ATR(mkt_data, n = 14)
#'
#' # Return only indicator columns
#' result2 <- add_ATR(mkt_data, n = 14, append = FALSE)
#'
#' # Use a longer period and return a plain data.frame
#' result3 <- add_ATR(mkt_data, n = 20, output = "data.frame")
#' }
#' @export
add_ATR <- function(mkt_data, n = 14, maType, append = TRUE,
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

    # Build HLC xts with uppercase column names expected by TTR internals
    hlc <- xts::xts(
      cbind(High = sub$high, Low = sub$low, Close = sub$close),
      order.by = sub$date
    )

    atr_val <- TTR::ATR(hlc, n = n, maType = maType, ...)
    sub[[paste0("tr_", n)]] <- as.numeric(atr_val[, "tr"])
    sub[[paste0("atr_", n)]] <- as.numeric(atr_val[, "atr"])
    sub[[paste0("trueHigh_", n)]] <- as.numeric(atr_val[, "trueHigh"])
    sub[[paste0("trueLow_", n)]] <- as.numeric(atr_val[, "trueLow"])
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original columns ──────────────────────────────────────
  if (!append) {
    new_cols <- c(
      paste0("tr_", n), paste0("atr_", n),
      paste0("trueHigh_", n), paste0("trueLow_", n)
    )
    keep <- intersect(c("date", "code", "name", new_cols), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
