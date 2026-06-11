#' @title Chaikin Money Flow (CMF)
#' @description
#' Computes Chaikin Money Flow for each security in a long-format panel data
#' frame. CMF compares the rolling sum of volume-weighted Close Location Values
#' to the rolling sum of total volume over \code{n} periods. Values above
#' \code{+0.25} suggest accumulation (bullish); values below \code{-0.25}
#' suggest distribution (bearish). Developed by Marc Chaikin.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, \code{low}, \code{close},
#'   and \code{volume}.
#' @param n Integer. Look-back window. Defaults to \code{20}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with column \code{CMF} containing values typically in
#'   \code{[-1, 1]}.
#' @export
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' mkt_data <- data.frame(
#'   date   = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 60), 2),
#'   code   = rep(c("AAPL", "MSFT"), each = 60),
#'   name   = rep(c("Apple", "Microsoft"), each = 60),
#'   high   = c(runif(60, 155, 205), runif(60, 305, 405)),
#'   low    = c(runif(60, 145, 195), runif(60, 295, 395)),
#'   close  = c(runif(60, 150, 200), runif(60, 300, 400)),
#'   volume = c(runif(60, 1e6, 2e6), runif(60, 5e5, 1.5e6))
#' )
#' # Example 1: Default parameters
#' result <- add_CMF(mkt_data)
#' # Example 2: Custom window
#' result <- add_CMF(mkt_data, n = 30)
#' # Example 3: Slim output as data.frame
#' result <- add_CMF(mkt_data, n = 20, append = FALSE, output = "data.frame")
#' }
add_CMF <- function(mkt_data, n = 20, append = TRUE, output = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, high, low, close, volume.")
  }
  required_cols <- c("date", "code", "high", "low", "close", "volume")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    hlc <- cbind(sub$high, sub$low, sub$close)
    vol <- sub$volume

    clv_val <- ((hlc[, 3] - hlc[, 2]) - (hlc[, 1] - hlc[, 3])) / (hlc[, 1] - hlc[, 2])
    clv_val[is.nan(clv_val) | is.infinite(clv_val)] <- 0

    cmf_val <- runSum(clv_val * vol, n) / runSum(vol, n)
    sub[["CMF"]] <- as.numeric(cmf_val)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  if (!append) {
    keep <- intersect(c("date", "code", "name", "CMF"), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  if (output == "tibble") {
    tibble::as_tibble(res)
  } else {
    as.data.frame(res, stringsAsFactors = FALSE)
  }
}
