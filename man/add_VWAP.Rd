% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_VWAP.R
\name{add_VWAP}
\alias{add_VWAP}
\title{Calculate Volume - Weighted Average Price (VWAP)}
\usage{
add_VWAP(OHLCV, n = 10, append = FALSE)
}
\arguments{
\item{OHLCV}{Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
The function will extract the closing price and volume from this object for VWAP calculation.}

\item{n}{Number of periods to average over. This parameter determines the window size for the VWAP calculation.
A larger \code{n} will result in a smoother VWAP, but it may be less responsive to recent price and volume changes.}

\item{append}{A logical value. If \code{TRUE}, the calculated VWAP values will be appended to the \code{OHLCV} input data,
ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
VWAP values will be returned. Defaults to \code{FALSE}.}
}
\value{
If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
containing the VWAP values.
If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated VWAP values appended,
maintaining the integrity of the time - series alignment.
}
\description{
Calculate the volume - weighted moving average price. The VWAP is a trading benchmark
that represents the average price at which a security has traded throughout the day,
weighted by the volume traded at each price level. It is commonly used by traders
to assess the fair value of a security during a specific period.
}
\examples{
\dontrun{
data(TSLA)
# Using default parameters without appending
vwap_result1 <- add_VWAP(TSLA)

# Using default parameters and appending
vwap_result2 <- add_VWAP(TSLA, append = TRUE)

# Changing n and without appending
vwap_result3 <- add_VWAP(TSLA, n = 15)

# Changing n and appending
vwap_result4 <- add_VWAP(TSLA, n = 15, append = TRUE)
}
}
\keyword{ts}
