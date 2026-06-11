#' Adjustment Ratios for Splits and Dividends
#'
#' Calculates price adjustment ratios to adjust historical prices for stock splits and dividends.
#'
#' @param splits Univariate xts object of stock split ratios.
#' @param dividends Univariate xts object of dividend payments.
#' @param close Univariate xts object of closing prices (required for dividend adjustments).
#'
#' @return Xts object with two columns: Split (adjustment ratio for splits) and Div (adjustment ratio for dividends).
#' @author DengYishuo
#' @keywords ts
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' adj <- adj_Ratios(splits, dividends, close)
#' }
adj_Ratios <-
  function(splits, dividends, close) {
    if (!missing(dividends) &&
      missing(close)) {
      stop('"close" must be specified to adjust dividends')
    }

    # Really need a better error message if as.xts fails... seriously
    if (missing(close) || all(is.na(close)) || NROW(close) == 0) {
      close <- NA
    } else {
      if (NCOL(close) != 1) stop('"close" must be univariate')
      close <- try.xts(close,
        error = stop('"as.xts(close)" failed')
      )
    }
    if (missing(splits) || all(is.na(splits)) || NROW(splits) == 0) {
      splits <- NA
    } else {
      if (NCOL(splits) != 1) stop('"splits" must be univariate')
      splits <- try.xts(splits,
        error = stop('"as.xts(splits)" failed')
      )
    }
    if (missing(dividends) || all(is.na(dividends)) || NROW(dividends) == 0) {
      dividends <- NA
    } else {
      if (NCOL(dividends) != 1) stop('"dividends" must be univariate')
      dividends <- try.xts(dividends,
        error = stop('"as.xts(dividends)" failed')
      )
    }

    obj <- merge.xts(close, splits, dividends)
    if (!isTRUE(is.na(close))) {
      obj <- obj[!is.na(obj[, 1]), ] # drop rows missing close prices
    }
    adj <- .Call(adjRatios, obj[, 2], obj[, 3], obj[, 1])
    adj <- xts(cbind(adj[[1]], adj[[2]]), index(obj))
    colnames(adj) <- c("Split", "Div")

    return(adj)
  }
