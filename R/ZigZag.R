#' Add Zig Zag Indicator (Panel Data)
#'
#' @param mkt_data Panel with date, code, high, low (or single price).
#' @param change Minimum percentage/price change. Default 10.
#' @param percent If TRUE, change is percentage. Default TRUE.
#' @param retrace Use retracement logic. Default FALSE.
#' @param lastExtreme Keep last extreme. Default TRUE.
#' @param append Logical.
#' @param output "tibble" or "data.frame".
#' @return Data frame with column ZigZag.
#' @export
add_ZigZag <- function(mkt_data, change = 10, percent = TRUE, retrace = FALSE,
                       lastExtreme = TRUE, append = TRUE,
                       output = c("tibble", "data.frame")) {
  output <- match.arg(output)
  required <- c("date", "code")
  if (!all(required %in% colnames(mkt_data))) stop("Need date and code")
  if (!("high" %in% colnames(mkt_data) && "low" %in% colnames(mkt_data)) &&
    !("close" %in% colnames(mkt_data))) {
    stop("Need either (high & low) or close column")
  }

  codes <- unique(mkt_data$code)
  res_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    if ("high" %in% colnames(sub) && "low" %in% colnames(sub)) {
      hl <- xts::xts(sub[, c("high", "low")], order.by = sub$date)
    } else {
      hl <- xts::xts(sub$close, order.by = sub$date)
    }
    zz <- ZigZag(hl,
      change = change, percent = percent,
      retrace = retrace, lastExtreme = lastExtreme
    )
    sub$ZigZag <- as.numeric(zz)
    sub
  })

  res <- do.call(rbind, res_list)
  res <- res[order(res$date, res$code), ]
  if (!append) res <- res[, intersect(c("date", "code", "name", "ZigZag"), colnames(res)), drop = FALSE]
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res)
}
