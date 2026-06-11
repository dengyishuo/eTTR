#' Add Adjustment Ratios for Splits and Dividends (Panel Data)
#'
#' @param mkt_data Panel with date, code, close.
#' @param splits Data frame or named list with split events: columns code, date, split_ratio.
#' @param dividends Data frame with dividends: columns code, date, dividend.
#' @param fill_value Value to fill for dates without events. Default 1 (no adjustment).
#' @param append Logical.
#' @param output "tibble" or "data.frame".
#' @return Data frame with columns adj_split, adj_div, adj_total.
#' @export
add_adj_Ratios <- function(mkt_data, splits = NULL, dividends = NULL,
                           fill_value = 1,
                           append = TRUE, output = c("tibble", "data.frame")) {
  output <- match.arg(output)
  required <- c("date", "code", "close")
  if (!all(required %in% colnames(mkt_data))) stop("Need date, code, close")

  # Convert events to xts per code
  codes <- unique(mkt_data$code)

  res_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    close_xts <- xts::xts(sub$close, order.by = sub$date)

    # Extract events for this code
    split_xts <- NULL
    if (!is.null(splits) && is.data.frame(splits)) {
      sp <- splits[splits$code == cd, c("date", "split_ratio")]
      if (nrow(sp) > 0) {
        split_xts <- xts::xts(sp$split_ratio, order.by = as.Date(sp$date))
      }
    }
    div_xts <- NULL
    if (!is.null(dividends) && is.data.frame(dividends)) {
      dv <- dividends[dividends$code == cd, c("date", "dividend")]
      if (nrow(dv) > 0) {
        div_xts <- xts::xts(dv$dividend, order.by = as.Date(dv$date))
      }
    }

    adj <- adj_Ratios(splits = split_xts, dividends = div_xts, close = close_xts)
    # adj is an xts with columns Split, Div
    # Align to sub dates
    sub$adj_split <- fill_value
    sub$adj_div <- fill_value
    common_idx <- match(index(adj), sub$date)
    sub$adj_split[common_idx] <- as.numeric(adj[, "Split"])
    sub$adj_div[common_idx] <- as.numeric(adj[, "Div"])
    sub$adj_total <- sub$adj_split * sub$adj_div
    sub
  })

  res <- do.call(rbind, res_list)
  res <- res[order(res$date, res$code), ]
  if (!append) {
    keep <- intersect(c("date", "code", "name", "adj_split", "adj_div", "adj_total"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res)
}
