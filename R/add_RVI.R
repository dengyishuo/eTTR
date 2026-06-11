#' @title Relative Vigor Index (RVI)
#' @description
#' Computes the Relative Vigor Index for each security in a long-format panel
#' data frame. RVI is a momentum oscillator that measures the strength of a
#' trend by comparing upward price movement standard deviations to total
#' movement. Rolling standard deviations are computed over \code{n} periods and
#' then smoothed with an EMA of length \code{ema.n}. Values range between 0 and
#' 100.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and \code{close}.
#' @param n Integer. Look-back window for rolling standard deviation. Defaults
#'   to \code{14}.
#' @param ema.n Integer. Look-back window for EMA smoothing applied to the
#'   rolling standard deviations. Defaults to \code{3}.
#' @param keepNA Logical. If \code{TRUE} (default), leading \code{NA} values
#'   are preserved in the output.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{RVI_\{n\}} containing values in
#'   \code{[0, 100]}.
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date  = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code  = rep(c("AAPL", "MSFT"), each = 60),
#'   name  = rep(c("Apple", "Microsoft"), each = 60),
#'   close = c(runif(60, 150, 200), runif(60, 300, 400))
#' )
#' # Example 1: Default parameters
#' result <- add_RVI(mkt_data)
#' # Example 2: Custom look-back window
#' result <- add_RVI(mkt_data, n = 20, ema.n = 5)
#' # Example 3: Slim output as data.frame
#' result <- add_RVI(mkt_data, append = FALSE, output = "data.frame")
#' }
add_RVI <- function(mkt_data, n = 14, ema.n = 3, keepNA = TRUE,
                    append = TRUE, output = c("tibble", "data.frame")) {
  # Argument resolution
  output <- match.arg(output)

  # Input validation
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }
  required_cols <- c("date", "code", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  col_name <- paste0("RVI_", n)

  # Split-apply-combine
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    close_xts <- xts::xts(sub$close, order.by = as.Date(sub$date))
    price_change <- diff(close_xts)
    # Prepend an NA row to align lengths
    price_change <- rbind(xts::xts(NA, order.by = as.Date(sub$date)[1]), price_change)

    up_change <- ifelse(price_change > 0, price_change, 0)
    down_change <- ifelse(price_change < 0, abs(price_change), 0)

    up_sd <- runSD(up_change, n = n)
    down_sd <- runSD(down_change, n = n)
    up_ema <- EMA(up_sd, n = ema.n)
    down_ema <- EMA(down_sd, n = ema.n)

    rvi_val <- 100 * (up_ema / (up_ema + down_ema))
    rvi_val[is.nan(rvi_val)] <- 0

    sub[[col_name]] <- as.numeric(rvi_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # Column selection
  if (!append) {
    keep <- intersect(c("date", "code", "name", col_name), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # Output format
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
