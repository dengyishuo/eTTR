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

#' @title Calculate Technical Analysis Pivot Points
#' @description
#' Computes pivot points (central pivot, resistance levels, and support levels)
#' based on OHLCV data using the standard 3-point pivot calculation method.
#'
#' @param data An xts object containing OHLCV price data
#' @param lagts Logical indicating whether to lag the results (default is TRUE)
#'
#' @return An xts object with the following columns:
#' \itemize{
#'   \item center: Central pivot point, calculated as (High + Low + Close)/3
#'   \item R1: First resistance level, (2*P) - Low
#'   \item R2: Second resistance level, P + (R1-S1)
#'   \item S1: First support level, (2*P) - High
#'   \item S2: Second support level, P - (R1-S1)
#' }
#'
#' @references
#' http://www.investopedia.com/articles/forex/05/FXpivots.asp
#' http://www.investopedia.com/articles/technical/04/041404.asp
#'
#' @author Brian G. Peterson
#' @export
#'
#' @examples
#' \dontrun{
#' # Load sample data
#' data(TSLA)
#' prices <- Cl(TSLA)
#'
#' # Calculate pivot points
#' pivot_points <- pivots(prices)
#' head(pivot_points)
#' }
pivots <- function(data, lagts = TRUE) {
  # Calculate central pivot
  center <- xts(rowSums(HLC(data)) / 3, order.by = index(data))

  # Calculate resistance and support levels
  R1 <- (2 * center) - Lo(data) # First resistance
  S1 <- (2 * center) - Hi(data) # First support
  R2 <- center + (R1 - S1) # Second resistance
  S2 <- center - (R1 - S1) # Second support

  # Combine results
  ret <- cbind(center, R1, R2, S1, S2)
  colnames(ret) <- c("center", "R1", "R2", "S1", "S2")

  # Optional lagging of results
  if (lagts) {
    newrow <- xts(t(rep(NA, 5)), order.by = last(index(data)) + 1)
    ret <- rbind(ret, newrow)
    ret <- lag.xts(ret)
  }

  return(ret)
}
