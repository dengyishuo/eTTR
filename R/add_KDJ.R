#' @title Add KDJ Stochastic Oscillator
#'
#' @description Computes the KDJ stochastic oscillator for each asset in a
#'   long-format panel data frame. The K line is a smoothed Raw Stochastic
#'   Value, D is a further smoothing of K, and J = 3K - 2D captures
#'   divergence. Results are appended as columns \code{K_<n>}, \code{D_<n>},
#'   and \code{J_<n>}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, \code{low}, and
#'   \code{close}.
#' @param n Integer. Look-back window for the rolling high-low range. Defaults
#'   to \code{9}.
#' @param m1 Integer. Smoothing factor denominator for the K line. Defaults to
#'   \code{3}.
#' @param m2 Integer. Smoothing factor denominator for the D line. Defaults to
#'   \code{3}.
#' @param fill_na_method Character. Method to fill leading NAs before the first
#'   valid RSV: \code{"none"} (default) leaves NAs, \code{"initial"} fills with
#'   50, \code{"interpolate"} uses linear interpolation.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return The input data frame with additional columns \code{K_<n>},
#'   \code{D_<n>}, and \code{J_<n>} containing the KDJ indicator values.
#'
#' @export
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date  = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code  = rep(c("AAPL", "MSFT"), each = 60),
#'   name  = rep(c("Apple", "Microsoft"), each = 60),
#'   high  = c(runif(60, 160, 210), runif(60, 310, 410)),
#'   low   = c(runif(60, 140, 190), runif(60, 290, 390)),
#'   close = c(runif(60, 150, 200), runif(60, 300, 400))
#' )
#' # Example 1: Default parameters
#' result <- add_KDJ(mkt_data)
#' # Example 2: Custom window and fill method
#' result <- add_KDJ(mkt_data, n = 14, fill_na_method = "initial")
#' # Example 3: Slim output
#' result <- add_KDJ(mkt_data, n = 9, append = FALSE)
#' }
add_KDJ <- function(mkt_data, n = 9, m1 = 3, m2 = 3, fill_na_method = "none",
                    append = TRUE, output = c("tibble", "data.frame")) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  fill_na_method <- match.arg(fill_na_method, c("none", "initial", "interpolate"))

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

    time_index <- sub$date
    len <- nrow(sub)

    high <- sub$high
    low <- sub$low
    close <- sub$close

    # Rolling highest high and lowest low
    highest_high <- zoo::rollapply(high, n, max, align = "right", fill = NA)
    lowest_low <- zoo::rollapply(low, n, min, align = "right", fill = NA)

    # Raw Stochastic Value
    rsv <- rep(NA_real_, len)
    valid_idx <- which(!is.na(highest_high) & !is.na(lowest_low) & (highest_high != lowest_low))
    zero_div_idx <- which(!is.na(highest_high) & !is.na(lowest_low) & (highest_high == lowest_low))
    rsv[valid_idx] <- 100 * (close[valid_idx] - lowest_low[valid_idx]) /
      (highest_high[valid_idx] - lowest_low[valid_idx])
    rsv[zero_div_idx] <- 50

    # Initialise K, D, J
    k <- rep(NA_real_, len)
    d <- rep(NA_real_, len)

    first_valid <- which(!is.na(rsv))[1]
    if (!is.na(first_valid)) {
      k[first_valid] <- 50
      d[first_valid] <- 50
      if ((first_valid + 1) <= len) {
        for (i in (first_valid + 1):len) {
          prev_k <- if (is.na(k[i - 1])) 50 else k[i - 1]
          prev_d <- if (is.na(d[i - 1])) 50 else d[i - 1]
          k[i] <- (1 / m1) * rsv[i] + (1 - 1 / m1) * prev_k
          d[i] <- (1 / m2) * k[i] + (1 - 1 / m2) * prev_d
        }
      }
    }

    j <- 3 * k - 2 * d

    # NA filling
    if (fill_na_method == "initial" && !is.na(first_valid) && first_valid > 1) {
      k[1:(first_valid - 1)] <- 50
      d[1:(first_valid - 1)] <- 50
      j[1:(first_valid - 1)] <- 50
    } else if (fill_na_method == "interpolate") {
      k <- as.numeric(zoo::na.approx(k, na.rm = FALSE))
      d <- as.numeric(zoo::na.approx(d, na.rm = FALSE))
      j <- 3 * k - 2 * d
    }

    sub[[paste0("K_", n)]] <- k
    sub[[paste0("D_", n)]] <- d
    sub[[paste0("J_", n)]] <- j
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original columns ──────────────────────────────────────
  if (!append) {
    new_cols <- c(paste0("K_", n), paste0("D_", n), paste0("J_", n))
    keep <- intersect(c("date", "code", "name", new_cols), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
