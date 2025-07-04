% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_DEMA.R
\name{add_DEMA}
\alias{add_DEMA}
\title{Calculate Double Exponential Moving Average (DEMA)}
\usage{
add_DEMA(OHLCV, n = 10, v = 1, wilder = FALSE, ratio = NULL, append = FALSE)
}
\arguments{
\item{OHLCV}{Object that is coercible to xts or matrix and contains Open - High - Low - Close - Volume data.
The function will extract the closing price from this object for DEMA calculation.}

\item{n}{Number of periods to average over. Must be between 1 and \code{nrow(OHLCV)}.
A larger \code{n} will result in a smoother DEMA, but it may be less responsive
to recent price changes.}

\item{v}{The 'volume factor' (a number in \code{[0,1]}; default is 1 (standard DEMA).
When \code{v = 1}, the function returns the standard DEMA. When \code{v = 0},
it returns the regular EMA. This parameter allows for adjusting the influence
of the second EMA calculation on the final DEMA value.}

\item{wilder}{logical; if \code{TRUE}, use Welles Wilder's EMA formula. Welles
Wilder's EMA formula gives more weight to recent data points in a specific way.}

\item{ratio}{A smoothing/decay ratio (overrides \code{wilder}). If provided,
this ratio is used to calculate the EMA instead of the \code{n}-based approach
(when \code{wilder} is \code{FALSE}) or the Welles Wilder's formula (when
\code{wilder} is \code{TRUE}).}

\item{append}{A logical value. If \code{TRUE}, the calculated DEMA values will be appended to the \code{OHLCV} input data,
ensuring proper alignment of time - series data. If \code{FALSE}, only the calculated
DEMA values will be returned. Defaults to \code{FALSE}.}
}
\value{
If \code{append} is \code{FALSE}, an object of the same class as \code{OHLCV} (or a vector if \code{try.xts} fails)
containing the DEMA values.
If \code{append} is \code{TRUE}, an object of the same class as \code{OHLCV} with the calculated DEMA values appended,
maintaining the integrity of the time - series alignment.
}
\description{
Calculate a double exponential moving average to reduce lag. The DEMA is designed
to provide a more responsive moving average compared to the simple exponential
moving average (EMA) by applying the EMA calculation twice.
}
\note{
When \code{v = 1}, returns standard DEMA; \code{v = 0} returns regular EMA.
}
\examples{
\dontrun{
data(TSLA)
# Using default parameters without appending
dema_result1 <- add_DEMA(TSLA)

# Using default parameters and appending
dema_result2 <- add_DEMA(TSLA, append = TRUE)

# Changing n and without appending
dema_result3 <- add_DEMA(TSLA, n = 15)

# Changing n and appending
dema_result4 <- add_DEMA(TSLA, n = 15, append = TRUE)
}
}
\keyword{ts}
