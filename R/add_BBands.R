#' Calculate Bollinger Bands
#'
#' Calculates Bollinger Bands for each security in a long-format panel data frame.
#' The middle band is an SMA of the typical price (high + low + close)/3.
#'
#' @param mkt_data A data.frame or tibble with columns: date, code, high, low, close.
#' @param n Moving average period. Default 20.
#' @param maType Moving average function. Default "SMA".
#' @param sd Number of standard deviations. Default 2.
#' @param append If TRUE, append columns to input data.
#' @param output Output format: "tibble" or "data.frame".
#' @param ... Additional parameters for moving average functions.
#'
#' @return Data frame with Bollinger Band columns added.
#' @export
#' @keywords ts
#' @author DengYishuo
#'
#' @examples
#' data(ettr_stocks)
#' result <- add_BBands(ettr_stocks)
add_BBands <- function(mkt_data, n = 20, maType, sd = 2, append = TRUE,
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

    bb <- TTR::BBands(hlc, n = n, sd = sd, ...)

    sub[[paste0("dn_", n)]] <- as.numeric(bb[, "dn"])
    sub[[paste0("mavg_", n)]] <- as.numeric(bb[, "mavg"])
    sub[[paste0("up_", n)]] <- as.numeric(bb[, "up"])
    sub[[paste0("pctB_", n)]] <- as.numeric(bb[, "pctB"])
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  if (!append) {
    new_cols <- c(
      paste0("dn_", n), paste0("mavg_", n),
      paste0("up_", n), paste0("pctB_", n)
    )
    keep <- intersect(c("date", "code", new_cols), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  if (output == "tibble") {
    tibble::as_tibble(res)
  } else {
    as.data.frame(res, stringsAsFactors = FALSE)
  }
}
