#' @title Stochastic Oscillator
#' @description
#' Computes the Stochastic Oscillator for each security in a long-format panel
#' data frame. Returns fast %K, fast %D (smoothed %K), and slow %D (smoothed
#' fast %D) for each ticker. The oscillator measures the position of the close
#' relative to the high-low range over the look-back period.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, \code{low}, and
#'   \code{close}.
#' @param nFastK Integer. Look-back window for fast %K. Defaults to \code{14}.
#' @param nFastD Integer. Smoothing periods for fast %D. Defaults to \code{3}.
#' @param nSlowD Integer. Smoothing periods for slow %D. Defaults to \code{3}.
#' @param maType Character. Moving average type applied to %K and %D. Defaults
#'   to \code{"SMA"}.
#' @param bounded Logical. If \code{TRUE} (default), values are bounded to
#'   \code{[0, 1]}.
#' @param smooth Integer. Smoothing applied to fast %K before computing fast %D.
#'   Defaults to \code{1} (no additional smoothing).
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the \code{maType} function.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with columns:
#'   \describe{
#'     \item{fastK_\{nFastK\}}{Fast %K values.}
#'     \item{fastD_\{nFastK\}}{Fast %D values (smoothed fast %K).}
#'     \item{slowD_\{nFastK\}}{Slow %D values (smoothed fast %D).}
#'   }
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date  = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code  = rep(c("AAPL", "MSFT"), each = 60),
#'   name  = rep(c("Apple", "Microsoft"), each = 60),
#'   high  = c(runif(60, 155, 205), runif(60, 305, 405)),
#'   low   = c(runif(60, 145, 195), runif(60, 295, 395)),
#'   close = c(runif(60, 150, 200), runif(60, 300, 400))
#' )
#' # Example 1: Default parameters
#' result <- add_stoch(mkt_data)
#' # Example 2: Custom look-back window
#' result <- add_stoch(mkt_data, nFastK = 20, nFastD = 5, nSlowD = 5)
#' # Example 3: Slim output as data.frame
#' result <- add_stoch(mkt_data, append = FALSE, output = "data.frame")
#' }
add_stoch <- function(mkt_data, nFastK = 14, nFastD = 3, nSlowD = 3,
                      maType, bounded = TRUE, smooth = 1, append = TRUE,
                      output = c("tibble", "data.frame"), ...) {
  output <- match.arg(output)
  if (missing(maType)) maType <- "SMA"

  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a data.frame or tibble.")
  }

  required_cols <- c("date", "code", "high", "low", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    hlc <- xts::xts(
      cbind(High = sub$high, Low = sub$low, Close = sub$close),
      order.by = sub$date
    )

    stoch_val <- stoch(
      hlc,
      nFastK = nFastK,
      nFastD = nFastD,
      nSlowD = nSlowD,
      maType = maType,
      bounded = bounded,
      smooth = smooth,
      ...
    )

    sub[[paste0("fastK_", nFastK)]] <- as.numeric(stoch_val[, "fastK"])
    sub[[paste0("fastD_", nFastK)]] <- as.numeric(stoch_val[, "fastD"])
    sub[[paste0("slowD_", nFastK)]] <- as.numeric(stoch_val[, "slowD"])
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  if (!append) {
    new_cols <- c(paste0("fastK_", nFastK), paste0("fastD_", nFastK), paste0("slowD_", nFastK))
    keep <- intersect(c("date", "code", new_cols), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  if (output == "tibble") {
    tibble::as_tibble(res)
  } else {
    as.data.frame(res, stringsAsFactors = FALSE)
  }
}
