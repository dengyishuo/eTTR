#' @title Add Know Sure Thing (KST) Oscillator
#'
#' @description Computes the Know Sure Thing momentum oscillator and its signal
#'   line for each asset in a long-format panel data frame. The KST is a
#'   weighted sum of four smoothed Rate-of-Change values. Results are appended
#'   as columns \code{KST_<n>} and \code{KSTsig_<n>}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and the required price columns.
#' @param n Integer vector of length 4. Look-back windows for the four smoothed
#'   ROC components. Defaults to \code{c(10, 10, 10, 15)}.
#' @param nROC Integer vector of length 4. Rate-of-change periods.
#'   Defaults to \code{c(10, 15, 20, 30)}.
#' @param nSig Integer. Signal line smoothing period. Defaults to \code{9}.
#' @param maType Character or list. Moving average type(s). Defaults to
#'   \code{"SMA"}.
#' @param wts Numeric vector. Weights for the four ROC components.
#'   Defaults to \code{1:NROW(n)}.
#' @param append Logical. If \code{TRUE} (default), append new columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#' @param ... Additional arguments passed to the moving average function.
#'
#' @return The input data frame with additional columns \code{KST_<n>} (the KST
#'   oscillator value) and \code{KSTsig_<n>} (the signal line).
#'
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date  = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 120), 2),
#'   code  = rep(c("AAPL", "MSFT"), each = 120),
#'   name  = rep(c("Apple", "Microsoft"), each = 120),
#'   close = c(runif(120, 150, 200), runif(120, 300, 400))
#' )
#' # Example 1: Default parameters
#' result <- add_KST(mkt_data)
#' # Example 2: Custom ROC periods
#' result <- add_KST(mkt_data, nROC = c(5, 10, 15, 20))
#' # Example 3: Slim output
#' result <- add_KST(mkt_data, append = FALSE)
#' }
add_KST <- function(mkt_data, n = c(10, 10, 10, 15), nROC = c(10, 15, 20, 30), nSig = 9,
                    maType, wts = 1:NROW(n), append = TRUE,
                    output = c("tibble", "data.frame"), ...) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)
  if (missing(maType)) maType <- "SMA"

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }
  required_cols <- c("date", "code", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }
  if (!isTRUE(all.equal(NROW(n), NROW(wts), NROW(nROC)))) {
    stop("'n', 'nROC', and 'wts' must be the same length.")
  }
  N <- NROW(n)

  # ── Column names ──────────────────────────────────────────────────────────
  n_label <- max(n)
  col_kst <- paste0("KST_", n_label)
  col_sig <- paste0("KSTsig_", n_label)

  # ── Split-apply-combine over each asset ───────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    min_rows <- max(nROC) + max(n) + nSig
    if (min_rows > nrow(sub)) {
      warning(sprintf("Skipping code '%s': insufficient rows for KST computation.", cd))
      sub[[col_kst]] <- NA_real_
      sub[[col_sig]] <- NA_real_
      return(sub)
    }

    close_xts <- xts::xts(sub$close, order.by = sub$date)
    ret <- NULL

    if (is.list(maType)) {
      maTypeInfo <- sapply(maType, is.list)
      if (!(all(maTypeInfo) && length(maTypeInfo) == N)) {
        stop("If 'maType' is a list, it must have the same number of elements as 'n'.")
      }
      for (i in seq_len(N)) {
        if (!is.null(formals(maType[[i]][[1]])$n) && is.null(maType[[i]]$n)) maType[[i]]$n <- n[i]
        roc <- ROC(close_xts, nROC[i], na.pad = TRUE)
        ma_roc <- do.call(maType[[i]][[1]], c(list(roc), maType[[i]][-1])) * wts[i]
        ret <- cbind(ret, ma_roc)
      }
    } else {
      for (i in seq_len(N)) {
        roc <- ROC(close_xts, nROC[i], na.pad = TRUE)
        ma_roc <- do.call(maType, c(list(roc), list(n = n[i], ...))) * wts[i]
        ret <- cbind(ret, ma_roc)
      }
    }

    kst <- xts::xts(100 * rowSums(ret), index(ret))

    if (is.list(maType)) {
      signal <- do.call(maType[[N]][[1]], c(list(kst), maType[[N]][-1]))
    } else {
      signal <- do.call(maType, c(list(kst), list(n = nSig, ...)))
    }

    sub[[col_kst]] <- as.numeric(kst)
    sub[[col_sig]] <- as.numeric(signal)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Slim output when append = FALSE ───────────────────────────────────────
  if (!append) {
    keep <- intersect(c("date", "code", "name", col_kst, col_sig), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format conversion ───────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
