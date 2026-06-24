#' Add Momentum (Panel Data)
#'
#' @param mkt_data Long-format panel with date, code, close.
#' @param n Number of periods for momentum. Default 1.
#' @param na.pad Pad leading NAs. Default TRUE.
#' @param append Logical.
#' @param output "tibble" or "data.frame".
#' @return Data frame with added column MOMENTUM_<n>.
#' @examples
#' data(ettr_stocks)
#' result <- add_momentum(ettr_stocks)
#' @export
add_momentum <- function(mkt_data, n = 1, na.pad = TRUE,
                         append = TRUE, output = c("tibble", "data.frame")) {
  output <- match.arg(output)
  if (!all(c("date", "code", "close") %in% colnames(mkt_data))) {
    stop("mkt_data needs columns: date, code, close")
  }

  col_name <- paste0("MOMENTUM_", n)
  codes <- unique(mkt_data$code)
  res_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    if (n >= nrow(sub)) {
      sub[[col_name]] <- NA_real_
      return(sub)
    }
    x <- sub$close
    mom <- c(rep(NA, n), diff(x, n))
    if (!na.pad) mom <- mom[-(1:n)]
    sub[[col_name]] <- mom
    sub
  })

  res <- do.call(rbind, res_list)
  res <- res[order(res$date, res$code), ]
  if (!append) res <- res[, intersect(c("date", "code", "name", col_name), colnames(res)), drop = FALSE]
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res)
}
