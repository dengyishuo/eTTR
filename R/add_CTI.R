#' @title Correlation Trend Indicator (CTI)
#' @description
#' Calculates the Correlation Trend Indicator for each security in a long-format
#' panel data frame. CTI measures the Spearman rank correlation between closing
#' prices and an idealized linear trend within a rolling window of length
#' \code{n}. Values near \code{1} indicate a strong uptrend; values near
#' \code{-1} indicate a strong downtrend; values near \code{0} indicate no
#' clear trend. Required column: \code{close}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and \code{close}.
#' @param n Integer. Look-back window. Defaults to \code{20}.
#' @param slope Numeric. Slope of the reference linear trend used as the
#'   ideal sequence against which \code{close} is correlated. Defaults to
#'   \code{1}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{CTI_<n>} containing the Spearman
#'   rank-correlation trend values in the range \code{[-1, 1]}.
#' @export
#' @importFrom xts xts
#' @importFrom zoo rollapplyr
#' @importFrom stats cor
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_CTI(ettr_stocks)
add_CTI <- function(mkt_data, n = 20, slope = 1, append = TRUE,
                    output = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }

  required_cols <- c("date", "code", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))

  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  col_name <- paste0("CTI_", n)
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
    y <- slope * seq_along(close_xts)

    f <- function(.) stats::cor(.[, 1], .[, 2], method = "spearman")
    cti <- zoo::rollapplyr(cbind(close_xts, y), n, f, by.column = FALSE, fill = NA)

    sub[[col_name]] <- as.numeric(cti)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  if (!append) {
    keep <- intersect(c("date", "code", "name", col_name), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  if (output == "tibble") {
    tibble::as_tibble(res)
  } else {
    as.data.frame(res, stringsAsFactors = FALSE)
  }
}
