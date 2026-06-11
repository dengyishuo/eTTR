#' @title Donchian Channel
#' @description
#' Computes the Donchian Channel for each security in a long-format panel data
#' frame. The channel is defined by the highest high and lowest low over a
#' rolling look-back window, with a midpoint midline.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, and \code{low}.
#' @param n Integer. Look-back window for rolling high and low. Defaults to
#'   \code{10}.
#' @param include.lag Logical. If \code{TRUE}, lag the channel by one period so
#'   the current bar is excluded from the calculation. Defaults to
#'   \code{FALSE}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with columns:
#'   \describe{
#'     \item{dc_high_\{n\}}{Upper Donchian Channel band (rolling \code{n}-period high).}
#'     \item{dc_mid_\{n\}}{Midpoint of the upper and lower bands.}
#'     \item{dc_low_\{n\}}{Lower Donchian Channel band (rolling \code{n}-period low).}
#'   }
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
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
#' result <- add_DonchianChannel(mkt_data)
#' # Example 2: Custom window
#' result <- add_DonchianChannel(mkt_data, n = 20)
#' # Example 3: Slim output with lag
#' result <- add_DonchianChannel(mkt_data, n = 10, include.lag = TRUE, append = FALSE)
#' }
add_DonchianChannel <- function(mkt_data, n = 10, include.lag = FALSE, append = TRUE,
                                output = c("tibble", "data.frame")) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, high, low.")
  }
  required_cols <- c("date", "code", "high", "low")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    # Build HL xts
    hl <- xts::xts(
      cbind(High = sub$high, Low = sub$low),
      order.by = sub$date
    )

    high_val <- runMax(hl[, 1], n)
    low_val  <- runMin(hl[, 2], n)
    mid_val  <- (high_val + low_val) / 2

    if (include.lag) {
      high_val <- xts::lag.xts(high_val)
      mid_val  <- xts::lag.xts(mid_val)
      low_val  <- xts::lag.xts(low_val)
    }

    sub[[paste0("dc_high_", n)]] <- as.numeric(high_val)
    sub[[paste0("dc_mid_",  n)]] <- as.numeric(mid_val)
    sub[[paste0("dc_low_",  n)]] <- as.numeric(low_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original columns ──────────────────────────────────────
  if (!append) {
    new_cols <- c(paste0("dc_high_", n), paste0("dc_mid_", n), paste0("dc_low_", n))
    keep <- intersect(c("date", "code", "name", new_cols), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
