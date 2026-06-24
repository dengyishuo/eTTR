#' Add Pivot Points (Panel Data)
#'
#' @param mkt_data Long-format panel with date, code, high, low, close.
#' @param lagts Logical, lag the results by one period. Default TRUE.
#' @param append Logical.
#' @param output "tibble" or "data.frame".
#' @return Data frame with columns pivot_center, R1, R2, S1, S2.
#' @examples
#' data(ettr_stocks)
#' result <- add_pivots(ettr_stocks)
#' @export
add_pivots <- function(mkt_data, lagts = TRUE,
                       append = TRUE, output = c("tibble", "data.frame")) {
  output <- match.arg(output)
  required <- c("date", "code", "high", "low", "close")
  if (!all(required %in% colnames(mkt_data))) stop("Missing required columns")

  codes <- unique(mkt_data$code)
  res_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    # Create xts of OHLC
    ohlc <- xts::xts(sub[, c("high", "low", "close")], order.by = sub$date)
    piv <- pivots(ohlc, lagts = lagts)
    piv <- piv[seq_len(nrow(sub)), ]   # align to sub rows (lagts adds an extra row)
    sub$pivot_center <- as.numeric(piv[, "center"])
    sub$R1 <- as.numeric(piv[, "R1"])
    sub$R2 <- as.numeric(piv[, "R2"])
    sub$S1 <- as.numeric(piv[, "S1"])
    sub$S2 <- as.numeric(piv[, "S2"])
    sub
  })

  res <- do.call(rbind, res_list)
  res <- res[order(res$date, res$code), ]
  if (!append) {
    keep <- intersect(c("date", "code", "name", "pivot_center", "R1", "R2", "S1", "S2"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res)
}
