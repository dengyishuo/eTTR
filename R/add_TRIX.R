#' @title Add Triple Exponential Average (TRIX) Oscillator
#'
#' @description Computes the TRIX oscillator (1-period rate of change of a
#'   triple-smoothed moving average) and its signal line for each asset in a
#'   long-format panel data frame. Results are appended as columns
#'   \code{TRIX_<n>} and \code{TRIXsig_<n>}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param n Integer. Look-back window for the triple smoothing. Defaults to
#'   \code{20}.
#' @param nSig Integer. Signal line smoothing period. Defaults to \code{9}.
#' @param maType Character or list. Moving average type. Defaults to
#'   \code{"EMA"}.
#' @param percent Logical. If \code{TRUE} (default), express TRIX as a
#'   percentage rate of change. If \code{FALSE}, return the absolute momentum.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the moving average function.
#'
#' @return The input data frame with additional columns \code{TRIX_<n>} (the
#'   TRIX oscillator) and \code{TRIXsig_<n>} (the signal line).
#'
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#'
#' @examples
#' data(ettr_stocks)
#' result <- add_TRIX(ettr_stocks)
add_TRIX <- function(mkt_data, n = 20, nSig = 9, maType, percent = TRUE, append = TRUE,
                     output = c("tibble", "data.frame"), ...) {

  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  if (missing(maType)) maType <- "EMA"

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }
  required_cols <- c("date", "code", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # ── Column names for outputs ───────────────────────────────────────────────
  col_trix <- paste0("TRIX_", n)
  col_sig  <- paste0("TRIXsig_", n)

  # ── Split-apply-combine over each asset ───────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    min_rows <- if (is.list(maType)) n * 3 + nSig else n * 3 + nSig
    if (min_rows > nrow(sub)) {
      warning(sprintf("Skipping code '%s': insufficient rows for TRIX computation.", cd))
      sub[[col_trix]] <- NA_real_
      sub[[col_sig]]  <- NA_real_
      return(sub)
    }

    close_xts <- xts::xts(sub$close, order.by = sub$date)

    # Compute triple-smoothed MA
    if (is.list(maType)) {
      maTypeInfo <- sapply(maType, is.list)
      if (!(all(maTypeInfo) && length(maTypeInfo) == 4)) {
        stop("If 'maType' is a list, you must specify four MAs.")
      }
      for (i in 1:3) {
        if (!is.null(formals(maType[[i]][[1]])$n) && is.null(maType[[i]]$n)) maType[[i]]$n <- n
      }
      if (!is.null(formals(maType[[4]][[1]])$n) && is.null(maType[[4]]$n)) maType[[4]]$n <- nSig
      mavg1 <- do.call(maType[[1]][[1]], c(list(close_xts), maType[[1]][-1]))
      mavg2 <- do.call(maType[[2]][[1]], c(list(mavg1),     maType[[2]][-1]))
      mavg3 <- do.call(maType[[3]][[1]], c(list(mavg2),     maType[[3]][-1]))
    } else {
      mavg1 <- do.call(maType, c(list(close_xts), list(n = n, ...)))
      mavg2 <- do.call(maType, c(list(mavg1),     list(n = n, ...)))
      mavg3 <- do.call(maType, c(list(mavg2),     list(n = n, ...)))
    }

    if (percent) {
      TRIX_val <- 100 * ROC(mavg3, n = 1, na.pad = TRUE, type = "discrete")
    } else {
      TRIX_val <- momentum(mavg3, n = 1, na.pad = TRUE)
    }

    if (is.list(maType)) {
      signal <- do.call(maType[[4]][[1]], c(list(TRIX_val), maType[[4]][-1]))
    } else {
      signal <- do.call(maType, c(list(TRIX_val), list(n = nSig, ...)))
    }

    sub[[col_trix]] <- as.numeric(TRIX_val)
    sub[[col_sig]]  <- as.numeric(signal)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Slim output when append = FALSE ───────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", col_trix, col_sig), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format conversion ───────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
