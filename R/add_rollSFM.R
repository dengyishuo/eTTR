#' Add Rolling Single-Factor Model (Panel Data)
#'
#' @param mkt_data Panel with date, code, and asset returns column (e.g., ret_asset).
#' @param Rb xts of factor returns (must have dates aligning with mkt_data dates). Could be a single series for all assets.
#' @param n Rolling window size. Default 60.
#' @param append Logical.
#' @param output "tibble" or "data.frame".
#' @return Data frame with columns alpha, beta, r.squared.
#' @export
add_rollSFM <- function(mkt_data, Rb, n = 60,
                        append = TRUE, output = c("tibble", "data.frame")) {
  output <- match.arg(output)
  if (!all(c("date", "code", "ret_asset") %in% colnames(mkt_data))) {
    stop("mkt_data needs date, code, ret_asset (asset returns)")
  }
  if (!xts::is.xts(Rb)) stop("Rb must be an xts object")

  codes <- unique(mkt_data$code)
  res_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    if (n >= nrow(sub)) {
      sub$alpha <- sub$beta <- sub$r.squared <- NA_real_
      return(sub)
    }
    Ra <- xts::xts(sub$ret_asset, order.by = sub$date)
    # Align Rb with Ra dates
    common <- merge(Ra, Rb, all = FALSE)
    if (nrow(common) < n) {
      sub$alpha <- sub$beta <- sub$r.squared <- NA_real_
      return(sub)
    }
    sfm <- rollSFM(common[, 1], common[, 2], n = n)
    # Align back to sub
    sub$alpha <- NA_real_
    sub$beta <- NA_real_
    sub$r.squared <- NA_real_
    idx_match <- match(index(sfm), sub$date)
    sub$alpha[idx_match] <- as.numeric(sfm$alpha)
    sub$beta[idx_match] <- as.numeric(sfm$beta)
    sub$r.squared[idx_match] <- as.numeric(sfm$r.squared)
    sub
  })

  res <- do.call(rbind, res_list)
  res <- res[order(res$date, res$code), ]
  if (!append) {
    keep <- intersect(c("date", "code", "name", "alpha", "beta", "r.squared"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res)
}
