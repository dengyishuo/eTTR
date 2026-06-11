#' Add Rate of Change (Panel Data)
#'
#' @param mkt_data Panel with date, code, close.
#' @param n Lag periods. Default 1.
#' @param type "continuous" (log diff) or "discrete" (ratio). Default "discrete".
#' @param na.pad Pad leading NAs. Default TRUE.
#' @param append Logical.
#' @param output "tibble" or "data.frame".
#' @return Data frame with column ROC_<n>.
#' @export
add_ROC <- function(mkt_data, n = 1, type = c("discrete", "continuous"),
                    na.pad = TRUE, append = TRUE, output = c("tibble", "data.frame")) {
  type <- match.arg(type)
  output <- match.arg(output)
  required <- c("date", "code", "close")
  if (!all(required %in% colnames(mkt_data))) stop("Need date, code, close")

  col_name <- paste0("ROC_", n)
  codes <- unique(mkt_data$code)
  res_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    if (n >= nrow(sub)) {
      sub[[col_name]] <- NA_real_
      return(sub)
    }
    x <- sub$close
    if (type == "discrete") {
      roc <- c(rep(NA, n), x[(n + 1):length(x)] / x[1:(length(x) - n)] - 1)
    } else {
      roc <- c(rep(NA, n), diff(log(x), n))
    }
    if (!na.pad) roc <- roc[-(1:n)]
    sub[[col_name]] <- roc
    sub
  })

  res <- do.call(rbind, res_list)
  res <- res[order(res$date, res$code), ]
  if (!append) res <- res[, intersect(c("date", "code", "name", col_name), colnames(res)), drop = FALSE]
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res)
}
