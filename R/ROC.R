#' Rate of Change (ROC)
#'
#' Calculate the Rate of Change (ROC) of a series. ROC can be computed as either
#' discrete (price ratio) or continuous (log difference).
#'
#' @param x Price series (numeric vector, matrix, or xts object).
#' @param n Number of periods for the ROC calculation. Default 1.
#' @param type Type of ROC: `"discrete"` (price ratio - 1) or `"continuous"` (log difference). Default `"discrete"`.
#' @param na.pad Logical. If `TRUE`, pad the result with NAs at the beginning. Default `TRUE`.
#'
#' @return An object of the same class as `x` containing the ROC values.
#'
#' @importFrom xts try.xts is.xts lag.xts reclass
#' @export
ROC <- function(x, n = 1, type = c("discrete", "continuous"), na.pad = TRUE) {
  # Rate of Change

  x <- try.xts(x, error = as.matrix)
  type <- match.arg(type)

  if (is.xts(x)) {
    if (type == "discrete") {
      roc <- x / lag.xts(x, n, na.pad = na.pad) - 1
    }
    # Continuous change
    if (type == "continuous") {
      roc <- diff(log(x), n, na.pad = na.pad)
    }
    # Convert back to original class
    reclass(roc, x)
  } else {
    NAs <- NULL
    if (na.pad) {
      NAs <- rep(NA, n)
    }
    # Discrete changes
    if (type == "discrete") {
      roc <- c(NAs, x[(n + 1):NROW(x)] / x[1:(NROW(x) - n)] - 1)
    }
    # Continuous changes
    if (type == "continuous") {
      roc <- c(NAs, diff(log(x), n))
    }
    return(roc)
  }
}
