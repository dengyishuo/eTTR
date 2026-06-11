#' Add Growth of $1 (Panel Data)
#'
#' @param mkt_data Panel with date, code, close.
#' @param signals Optional numeric vector same length as mkt_data (or column name) for signal weighting.
#' @param ... Additional args to ROC (e.g., n, type).
#' @param append Logical.
#' @param output "tibble" or "data.frame".
#' @return Data frame with column GROWTH.
#' @export
add_growth <- function(mkt_data, signals = NULL, ...,
                       append = TRUE, output = c("tibble", "data.frame")) {
  output <- match.arg(output)
  if (!all(c("date", "code", "close") %in% colnames(mkt_data))) stop("Need date, code, close")

  codes <- unique(mkt_data$code)
  res_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    p <- sub$close
    if (!is.null(signals)) {
      if (is.character(signals) && signals %in% colnames(sub)) {
        sig <- sub[[signals]]
      } else if (length(signals) == nrow(sub)) {
        sig <- signals
      } else {
        stop("signals must be column name or vector of same length as asset data")
      }
    } else {
      sig <- rep(1, nrow(sub))
    }
    # growth function requires ROC from TTR? Use internal ROC from package if needed
    # Here we compute cumulative product of (1 + ROC(price) * signals)
    roc <- ROC(p, ...)
    growth_val <- cumprod(1 + roc * sig)
    sub$GROWTH <- as.numeric(growth_val)
    sub
  })

  res <- do.call(rbind, res_list)
  res <- res[order(res$date, res$code), ]
  if (!append) res <- res[, intersect(c("date", "code", "name", "GROWTH"), colnames(res)), drop = FALSE]
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res)
}
