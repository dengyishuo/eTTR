#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2025 - 2030  DengYishuo
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#' @title Calculate Know Sure Thing
#' @description
#' The Know Sure Thing (KST) is a smooth, summed, rate of change indicator.
#' Developed by Martin Pring.
#' For each day (week, month, etc.), the KST calculates the ROC over several
#' periods. Those ROCs are smoothed using the given moving averages, then
#' multiplied by their respective weighting values. The resulting values are
#' summed for each day (month, week, etc.).
#' @param OHLCV Price series that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.
#' @param n A vector of the number of periods to use in the MA calculations. Defaults to c(10, 10, 10, 15).
#' @param nROC A vector of the number of periods to use in the ROC calculations. Defaults to c(10, 15, 20, 30).
#' @param nSig The number of periods to use for the KST signal line. Defaults to 9.
#' @param maType Either:
#'  \enumerate{
#'    \item A function or a string naming the function to be called.
#'    \item A \emph{list} with the first component like (1) above, and
#'      additional parameters specified as \emph{named} components.
#'      See Examples.
#'  }
#' @param wts A vector the same length as \code{n}, of the weight for each
#' period (need not sum to one). Defaults to 1:NROW(n).
#' @param append A logical value. If \code{TRUE}, the calculated Know Sure Thing
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time - series data. If \code{FALSE}, only the calculated
#' Know Sure Thing values will be returned. Defaults to \code{FALSE}.
#' @param ... Other arguments to be passed to the \code{maType} function in
#' case (1) above.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the Know Sure Thing values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated Know Sure Thing values appended, maintaining the integrity of the time - series
#' alignment.
#' @note The KST indicates bullish/bearish momentum as it crosses above/below
#' its moving average. Because the KST tends to lead price action, look for
#' trend confirmation in the price.
#' The default arguments are for the daily KST. There is also the Long - Term
#' KST, with arguments: \code{n=c(9, 12, 18, 24)} - where the periods are
#' months, not days - and the moving average periods are 6, 6, 6, and 9 months,
#' respectively.
#' @author DengYishuo
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.  See \code{\link{ROC}} for the
#' rate - of - change function.  See \code{\link{MACD}} for a generic oscillator.
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://web.archive.org/web/20110715112957/http://www.pring.com/movieweb/daily_kst.htm}\cr
#' \url{https://web.archive.org/web/20100101162707/http://www.pring.com/movieweb/KST_MCM.htm}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' # Using default parameters without appending
#' kst_result1 <- add_KST(TSLA)
#'
#' # Modifying n and without appending
#' kst_result2 <- add_KST(TSLA, n = c(12, 12, 12, 18))
#'
#' # Using default parameters and appending
#' kst_result3 <- add_KST(TSLA, append = TRUE)
#'
#' # Modifying n and appending
#' kst_result4 <- add_KST(TSLA, n = c(12, 12, 12, 18), append = TRUE)
#' }
add_KST <- function(OHLCV, n = c(10, 10, 10, 15), nROC = c(10, 15, 20, 30), nSig = 9,
                    maType, wts = 1:NROW(n), append = FALSE, ...) {
  # Assume we use Close price for calculation, can be adjusted
  price <- OHLCV[, "Close"]

  if (!all.equal(NROW(n), NROW(wts), NROW(nROC))) {
    stop("'n', 'nROC', and 'wts' must be the same length.")
  } else {
    N <- NROW(n)
  }

  # Default MA
  if (missing(maType)) {
    maType <- "SMA"
  }

  ret <- NULL

  # Case of two different 'maType's for both MAs.
  if (is.list(maType)) {
    # Make sure maType is a list of lists
    maTypeInfo <- sapply(maType, is.list)
    if (!(all(maTypeInfo) && length(maTypeInfo) == N)) {
      stop(
        "If \'maType\' is a list, you must specify\n ",
        "the same number of MAs as elements in \'n\' and\n ",
        "\'nROC\' (see Examples section of ?KST)"
      )
    }

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with formal 'n'
    for (i in 1:length(maType)) {
      if (!is.null(formals(maType[[i]][[1]])$n) && is.null(maType[[i]]$n)) {
        maType[[i]]$n <- n[i]
      }
      roc <- ROC(price, nROC[i], na.pad = TRUE)
      ma.roc <- do.call(maType[[i]][[1]], c(list(roc), maType[[i]][-1])) * wts[i]
      ret <- cbind(ret, ma.roc)
    }
  }

  # Case of one 'maType' for both MAs.
  else {
    for (i in 1:NROW(n)) {
      roc <- ROC(price, nROC[i], na.pad = TRUE)
      ma.roc <- do.call(maType, c(list(roc), list(n = n[i], ...))) * wts[i]
      ret <- cbind(ret, ma.roc)
    }
  }

  if (is.xts(ret)) {
    kst <- xts(100 * rowSums(ret), index(ret))
  } else {
    kst <- 100 * rowSums(ret)
  }

  if (is.list(maType)) {
    sigMA <- length(maType)
    signal <- do.call(maType[[sigMA]][[1]], c(list(kst), maType[[sigMA]][-1]))
  } else {
    signal <- do.call(maType, c(list(kst), list(n = nSig, ...)))
  }

  result <- cbind(kst, signal)
  colnames(result) <- c("kst", "signal")
  result <- reclass(result, price)

  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    combined_result <- cbind(ohlcv, result)
    return(combined_result)
  } else {
    return(result)
  }
}
