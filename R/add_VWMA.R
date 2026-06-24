#' Add Volume-Weighted Moving Average (Panel Data)
#'
#' @param mkt_data Panel with date, code, close, volume.
#' @param n Window length. Default 10.
#' @param append Logical.
#' @param output "tibble" or "data.frame".
#' @return Data frame with column VWMA_<n>.
#' @importFrom TTR  VWMA
#' @examples
#' data(ettr_stocks)
#' result <- add_VWMA(ettr_stocks)
#' @export
add_VWMA <- function(mkt_data, n = 10,
                     append = TRUE, output = c("tibble", "data.frame")) {
  output <- match.arg(output)
  if (!all(c("date", "code", "close", "volume") %in% colnames(mkt_data))) {
    stop("Need date, code, close, volume")
  }

  col_name <- paste0("VWMA_", n)
  codes <- unique(mkt_data$code)
  res_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    if (n > nrow(sub)) {
      sub[[col_name]] <- NA_real_
      return(sub)
    }
    vwma <- TTR::VWMA(sub$close, sub$volume, n = n)
    sub[[col_name]] <- as.numeric(vwma)
    sub
  })

  res <- do.call(rbind, res_list)
  res <- res[order(res$date, res$code), ]
  if (!append) res <- res[, intersect(c("date", "code", "name", col_name), colnames(res)), drop = FALSE]
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res)
}
