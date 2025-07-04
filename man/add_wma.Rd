% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_WMA.R
\name{add_WMA}
\alias{add_WMA}
\title{Calculate Weighted Moving Average (WMA)}
\usage{
add_WMA(OHLCV, n = 10, wts = 1:n, append = FALSE)
}
\arguments{
\item{OHLCV}{Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
The function will extract the closing price from this object for WMA calculation.}

\item{n}{Number of periods to average over. Must be between 1 and \code{nrow(OHLCV)}.
A larger \code{n} will result in a smoother WMA, but it may be less responsive to recent price changes.}

\item{wts}{Vector of weights (length must equal \code{n} or length of \code{OHLCV}).
These weights determine the importance of each data point in the moving average calculation.}

\item{append}{A logical value. If \code{TRUE}, the calculated WMA values will be appended to the \code{OHLCV} input data,
ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
WMA values will be returned. Defaults to \code{FALSE}.}
}
\value{
If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
containing the WMA values.
If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated WMA values appended,
maintaining the integrity of the time - series alignment.
}
\description{
Calculate a weighted moving average with custom weights. The WMA assigns different
weights to each data point in the series, allowing for more flexibility in
emphasizing recent or past data.
}
\examples{
\dontrun{
data(TSLA)
# Using default parameters without appending
wma_result1 <- add_WMA(TSLA)

# Using default parameters and appending
wma_result2 <- add_WMA(TSLA, append = TRUE)

# Changing n and without appending
wma_result3 <- add_WMA(TSLA, n = 15)

# Changing n and appending
wma_result4 <- add_WMA(TSLA, n = 15, append = TRUE)
}
}
\keyword{ts}
