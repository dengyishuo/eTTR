#' @title Add Hull Moving Average (HMA)
#'
#' @description Computes the Hull Moving Average, defined as
#'   \code{WMA(2*WMA(close, n/2) - WMA(close, n), sqrt(n))}, for each asset in
#'   a long-format panel data frame and appends the result as a new column named
#'   \code{HMA_<n>}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param n Integer. Look-back window. Defaults to \code{20}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return The input data frame with an additional column \code{HMA_<n>}
#'   containing the Hull moving average of \code{close}.
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
#' result <- add_HMA(mkt_data)
#' # Example 2: Custom window
#' result <- add_HMA(mkt_data, n = 30)
#' # Example 3: Slim output
#' result <- add_HMA(mkt_data, n = 50, append = FALSE)
#' }
add_HMA <- function(mkt_data, n = 20, append = TRUE, output = c("tibble", "data.frame")) {

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
  if (n < 1) stop("n must be a positive integer.")

  # ── Split-apply-combine over each asset ───────────────────────────────────
  col_name <- paste0("HMA_", n)
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

    # HMA = WMA(2*WMA(x, n/2) - WMA(x, n), sqrt(n))
    madiff <- 2 * WMA(close_xts, n = trunc(n / 2)) - WMA(close_xts, n = n)
    hma <- WMA(madiff, n = trunc(sqrt(n)))
    sub[[col_name]] <- as.numeric(hma)
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
