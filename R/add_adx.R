#' @title Average Directional Index (ADX)
#' @description
#' Calculates the Directional Movement Index (DIp, DIn, DX) and Average Directional
#' Index (ADX) for each security in a long-format panel data frame.
#' Developed by J. Welles Wilder.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, \code{low}, and
#'   \code{close}.
#' @param n Integer. Look-back window for directional movement smoothing.
#'   Defaults to \code{14}.
#' @param maType Moving average type used for ADX smoothing. Defaults to
#'   Wilder's EMA (\code{"EMA"} with \code{wilder = TRUE}).
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the \code{maType} function.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with columns:
#'   \describe{
#'     \item{DIp_\{n\}}{Positive Directional Index (+DI).}
#'     \item{DIn_\{n\}}{Negative Directional Index (-DI).}
#'     \item{DX_\{n\}}{Directional Index (DX), the precursor to ADX.}
#'     \item{ADX_\{n\}}{Average Directional Index measuring trend strength
#'       (values above 25 typically indicate a strong trend).}
#'   }
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @importFrom TTR ADX
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
#' result <- add_adx(mkt_data)
#' # Example 2: Custom window
#' result <- add_adx(mkt_data, n = 20, append = FALSE)
#' # Example 3: Return as data.frame
#' result <- add_adx(mkt_data, n = 14, output = "data.frame")
#' }
add_adx <- function(mkt_data, n = 14, maType, append = TRUE,
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

    # Use ADX; pass maType and wilder flag for Wilder EMA default
    ma_args <- list(n = n, ...)
    if (identical(maType, "EMA") && is.null(ma_args$wilder)) {
      ma_args$wilder <- TRUE
    }
    adx_val <- TTR::ADX(hlc, n = n, maType = maType, ...)

    sub[[paste0("DIp_", n)]] <- as.numeric(adx_val[, "DIp"])
    sub[[paste0("DIn_", n)]] <- as.numeric(adx_val[, "DIn"])
    sub[[paste0("DX_", n)]] <- as.numeric(adx_val[, "DX"])
    sub[[paste0("ADX_", n)]] <- as.numeric(adx_val[, "ADX"])
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original columns ──────────────────────────────────────
  if (!append) {
    new_cols <- c(
      paste0("DIp_", n), paste0("DIn_", n),
      paste0("DX_", n), paste0("ADX_", n)
    )
    keep <- intersect(c("date", "code", "name", new_cols), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
