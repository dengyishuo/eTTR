% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_HMA.R
\name{add_HMA}
\alias{add_HMA}
\title{Calculate Hull Moving Average (HMA)}
\usage{
add_HMA(OHLCV, n = 20, append = FALSE)
}
\arguments{
\item{OHLCV}{Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
The function will extract the closing price from this object for HMA calculation.}

\item{n}{Number of periods to average over. This parameter determines the overall
window size for the HMA calculation. A larger \code{n} will result in a smoother
HMA, but it may be less responsive to short - term price changes.}

\item{append}{A logical value. If \code{TRUE}, the calculated HMA values will be appended to the \code{OHLCV} input data,
ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
HMA values will be returned. Defaults to \code{FALSE}.}
}
\value{
If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
containing the HMA values.
If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated HMA values appended,
maintaining the integrity of the time - series alignment.
}
\description{
Calculate a highly responsive moving average with reduced lag. The Hull Moving
Average (HMA) is designed to provide a more accurate representation of price
trends by reducing the lag typically associated with traditional moving averages.
It does this by using a weighted combination of different - period moving averages.
}
\examples{
\dontrun{
data(TSLA)
# Using default parameters without appending
hma_result1 <- add_HMA(TSLA)

# Using default parameters and appending
hma_result2 <- add_HMA(TSLA, append = TRUE)

# Changing n and without appending
hma_result3 <- add_HMA(TSLA, n = 15)

# Changing n and appending
hma_result4 <- add_HMA(TSLA, n = 15, append = TRUE)
}
}
\keyword{ts}
