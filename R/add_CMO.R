#' @title Chande Momentum Oscillator (CMO)
#' @description
#' Computes the Chande Momentum Oscillator for each security in a long-format
#' panel data frame. CMO measures momentum by comparing the sum of recent gains
#' to the sum of recent losses over a look-back period, returning values between
#' -100 and +100. Developed by Tushar Chande.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, and \code{close}.
#' @param n Integer. Look-back window. Defaults to \code{14}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{CMO_\{n\}} containing values in the range
#'   \code{[-100, 100]}.
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @examples
#' data(ettr_stocks)
#' result <- add_CMO(ettr_stocks)
add_CMO <- function(mkt_data, n = 14, append = TRUE, output = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, close.")
  }

  required_cols <- c("date", "code", "close")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  col_name <- paste0("CMO_", n)
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

    up <- momentum(close_xts, n = 1)
    dn <- ifelse(up < 0, abs(up), 0)
    up <- ifelse(up > 0, up, 0)
    up <- TTR::runSum(up, n)
    dn <- TTR::runSum(dn, n)
    cmo <- 100 * (up - dn) / (up + dn)

    sub[[col_name]] <- as.numeric(cmo)
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
