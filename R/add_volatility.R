#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2025-2030  DengYishuo
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
#' @title Calculate Volatility
#' @description
#' Selected volatility estimators/indicators; various authors.
#' @param OHLCV Object that is coercible to xts or matrix and contains
#' Open-High-Low-Close prices (or only Close prices, if \code{calc="close"}).
#' @param n Number of periods for the volatility estimate. Default is 10.
#' @param calc The calculation (type) of estimator to use.
#' @param N Number of periods per year. Default is 260.
#' @param mean0 Use a mean of 0 rather than the sample mean. Default is FALSE.
#' @param append A logical value. If \code{TRUE}, the calculated volatility
#' values will be appended to the \code{OHLCV} input data, ensuring
#' proper alignment of time-series data. If \code{FALSE}, only the calculated
#' volatility values will be returned. Defaults to \code{FALSE}.
#' @param ... Arguments to be passed to/from other methods.
#' @return If \code{append = FALSE}, an object of the same class as \code{OHLCV}
#' or a vector (if \code{try.xts} fails) containing the chosen volatility estimator values.
#' If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
#' calculated volatility values appended, maintaining the integrity of the time-series
#' alignment.
#' @author DengYishuo
#' @seealso See \code{\link{TR}} and \code{\link{chaikinVolatility}} for other
#' volatility measures.
#' @references The following sites were used to code/document these
#' indicators. All were created by Thijs van den Berg under the GNU Free
#' Documentation License and were retrieved on 2008-04-20. The original
#' links are dead, but can be accessed via internet archives.\cr
#' \cr Close-to-Close Volatility (\code{calc="close"}):\cr
#' \url{https://web.archive.org/web/20100421083157/http://www.sitmo.com/eq/172}\cr
#' \cr OHLC Volatility: Garman Klass (\code{calc="garman.klass"}):\cr
#' \url{https://web.archive.org/web/20100326172550/http://www.sitmo.com/eq/402}\cr
#' \cr High-Low Volatility: Parkinson (\code{calc="parkinson"}):\cr
#' \url{https://web.archive.org/web/20100328195855/http://www.sitmo.com/eq/173}\cr
#' \cr OHLC Volatility: Rogers Satchell (\code{calc="rogers.satchell"}):\cr
#' \url{https://web.archive.org/web/20091002233833/http://www.sitmo.com/eq/414}\cr
#' \cr OHLC Volatility: Garman Klass - Yang Zhang (\code{calc="gk.yz"}):\cr
#' \url{https://web.archive.org/web/20100326215050/http://www.sitmo.com/eq/409}\cr
#' \cr OHLC Volatility: Yang Zhang (\code{calc="yang.zhang"}):\cr
#' \url{https://web.archive.org/web/20100326215050/http://www.sitmo.com/eq/409}\cr
#' @keywords ts
#' @export
#' @examples
#' \dontrun{
#' data(TSLA)
#' ohlcv <- TSLA[, c("Open", "High", "Low", "Close")]
#' # Using default parameters without appending
#' vClose <- add_volatility(ohlcv, calc = "close")
#' # Modifying n and without appending
#' vClose0 <- add_volatility(ohlcv, calc = "close", n = 15)
#' # Using default parameters and appending
#' vGK <- add_volatility(ohlcv, calc = "garman", append = TRUE)
#' # Modifying N and appending
#' vParkinson <- add_volatility(ohlcv, calc = "parkinson", N = 250, append = TRUE)
#' # Using default parameters and appending for Rogers-Satchell
#' vRS <- add_volatility(ohlcv, calc = "rogers", append = TRUE)
#' }
add_volatility <- function(OHLCV, n = 10, calc = "close", N = 260, mean0 = FALSE, append = FALSE, ...) {
  # Convert input to xts or matrix
  OHLC <- try.xts(OHLCV[, c("Open", "High", "Low", "Close")], error = as.matrix)

  # Validate calc parameter
  calc <- match.arg(
    calc,
    c(
      "close", "garman.klass", "parkinson",
      "rogers.satchell", "gk.yz", "yang.zhang"
    )
  )

  # Historical Close-to-Close Volatility
  if (calc == "close") {
    price <- if (NCOL(OHLC) == 1) OHLC else OHLC[, "Close"]
    r <- ROC(price, 1, ...)
    s <- if (mean0) {
      sqrt(N) * sqrt(runSum(r^2, n - 1) / (n - 2))
    } else {
      sqrt(N) * runSD(r, n - 1)
    }
  }

  # Historical Open-High-Low-Close Volatility: Garman Klass
  if (calc == "garman.klass") {
    s <- sqrt(N / n * runSum(
      0.5 * log(OHLC[, "High"] / OHLC[, "Low"])^2 -
        (2 * log(2) - 1) * log(OHLC[, "Close"] / OHLC[, "Open"])^2,
      n
    ))
  }

  # Historical High-Low Volatility: Parkinson
  if (calc == "parkinson") {
    s <- sqrt(N / (4 * n * log(2)) * runSum(
      log(OHLC[, "High"] / OHLC[, "Low"])^2,
      n
    ))
  }

  # Historical Open-High-Low-Close Volatility: Rogers Satchell
  if (calc == "rogers.satchell") {
    s <- sqrt(N / n * runSum(
      log(OHLC[, "High"] / OHLC[, "Close"]) * log(OHLC[, "High"] / OHLC[, "Open"]) +
        log(OHLC[, "Low"] / OHLC[, "Close"]) * log(OHLC[, "Low"] / OHLC[, "Open"]),
      n
    ))
  }

  # Historical Open-High-Low-Close Volatility: Garman and Klass (Yang Zhang)
  if (calc == "gk.yz") {
    Cl1 <- lag.xts(OHLC[, "Close"])
    s <- sqrt(N / n * runSum(
      log(OHLC[, "Open"] / Cl1)^2 +
        0.5 * log(OHLC[, "High"] / OHLC[, "Low"])^2 -
        (2 * log(2) - 1) * log(OHLC[, "Close"] / OHLC[, "Open"])^2,
      n
    ))
  }

  # Historical Open-High-Low-Close Volatility: Yang Zhang
  if (calc == "yang.zhang") {
    Cl1 <- lag.xts(OHLC[, "Close"])
    dots <- list(...)
    alpha <- if (is.null(dots$alpha)) 1.34 else dots$alpha
    k <- if (is.null(dots$k)) (alpha - 1) / (alpha + (n + 1) / (n - 1)) else dots$k

    s2o <- N * runVar(log(OHLC[, "Open"] / Cl1), n = n)
    s2c <- N * runVar(log(OHLC[, "Close"] / OHLC[, "Open"]), n = n)
    s2rs <- add_volatility(OHLC, n = n, calc = "rogers.satchell", N = N, ...)
    s <- sqrt(s2o + k * s2c + (1 - k) * (s2rs^2))
  }

  # Return result
  if (append) {
    ohlcv <- try.xts(OHLCV, error = as.matrix)
    colname <- paste("volatility", calc, sep = ".")
    combined <- cbind(ohlcv, setNames(s, colname))
    return(combined)
  } else {
    return(s)
  }
}
