#' Pivot Points
#'
#' Calculate standard pivot points (central pivot, support and resistance levels)
#' based on the high, low, and closing prices.
#'
#' @param data An xts object containing at least columns `High`, `Low`, `Close`
#'   (case‑sensitive). Typically, the output of `quantmod::HLC()`.
#' @param lagts Logical. If `TRUE`, the results are lagged by one period so that
#'   today's pivots are based on yesterday's data. Default `TRUE`.
#'
#' @return An xts object with columns: `center` (pivot), `R1`, `R2`, `S1`, `S2`.
#'
#' @importFrom xts xts last lag.xts
#' @importFrom quantmod HLC Lo Hi
#' @importFrom zoo index
#' @export
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
