#' @title Add Trend Detection Index (TDI)
#'
#' @description Computes the Trend Detection Index and its Direction Indicator
#'   for each asset in a long-format panel data frame. The TDI identifies
#'   whether the market is trending or trading in a range by comparing
#'   directional momentum sums over two window lengths. Results are appended as
#'   columns \code{TDI_<n>} and \code{DI_<n>}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param n Integer. Primary look-back window. Defaults to \code{20}.
#' @param multiple Integer. Multiplier applied to \code{n} to form the second
#'   (longer) window. Defaults to \code{2}.
#' @param allow_middle_na Logical. If \code{FALSE} (default), non-leading NAs
#'   in the price series raise an error. Set to \code{TRUE} to override.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return The input data frame with additional columns \code{TDI_<n>} (the
#'   Trend Detection Index) and \code{DI_<n>} (the Direction Indicator).
#'
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#'
#' @examples
#' data(ettr_stocks)
#' result <- add_TDI(ettr_stocks)
add_TDI <- function(mkt_data, n = 20, multiple = 2, allow_middle_na = FALSE,
                    append = TRUE, output = c("tibble", "data.frame")) {
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
  if (n <= 0 || multiple <= 0) stop("n and multiple must be positive")
  if (!is.logical(allow_middle_na) || length(allow_middle_na) != 1) {
    stop("allow_middle_na must be a logical value")
  }

  # ── Column names ──────────────────────────────────────────────────────────
  col_tdi <- paste0("TDI_", n)
  col_di <- paste0("DI_", n)

  # ── Split-apply-combine over each asset ───────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    if (n >= nrow(sub)) {
      warning(sprintf("Skipping code '%s': n = %d >= available rows (%d).", cd, n, nrow(sub)))
      sub[[col_tdi]] <- NA_real_
      sub[[col_di]] <- NA_real_
      return(sub)
    }

    close_xts <- xts::xts(sub$close, order.by = sub$date)
    price_len <- nrow(close_xts)

    if (anyNA(close_xts)) {
      first_non_na <- min(which(!is.na(close_xts)))
      if (!allow_middle_na && anyNA(close_xts[first_non_na:price_len])) {
        stop(sprintf(
          "TDI for code '%s': non-leading NAs found. Set allow_middle_na = TRUE to override.", cd
        ))
      }
      if (first_non_na > 1) {
        close_xts <- close_xts[first_non_na:price_len]
        price_len <- nrow(close_xts)
      }
    }

    mom <- momentum(close_xts, n, na.pad = TRUE)
    mom[is.na(mom)] <- 0

    di <- TTR::runSum(mom, n)
    abs_di <- abs(di)

    run_sum_2n <- min(n * multiple, price_len - n + 1)
    run_sum_2n <- max(run_sum_2n, 1)
    run_sum_1n <- max(n, 1)

    abs_mom_2n <- TTR::runSum(abs(mom), run_sum_2n)
    abs_mom_1n <- TTR::runSum(abs(mom), run_sum_1n)
    tdi <- abs_di - (abs_mom_2n - abs_mom_1n)

    # Re-align to original sub if leading NAs were stripped
    tdi_vec <- as.numeric(tdi)
    di_vec <- as.numeric(di)
    n_orig <- nrow(sub)
    n_calc <- length(tdi_vec)
    if (n_calc < n_orig) {
      tdi_vec <- c(rep(NA_real_, n_orig - n_calc), tdi_vec)
      di_vec <- c(rep(NA_real_, n_orig - n_calc), di_vec)
    }

    sub[[col_tdi]] <- tdi_vec
    sub[[col_di]] <- di_vec
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Slim output when append = FALSE ───────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", col_tdi, col_di), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format conversion ───────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
