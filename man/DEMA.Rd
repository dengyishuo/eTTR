% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DEMA.R
\name{DEMA}
\alias{DEMA}
\title{Double Exponential Moving Average (DEMA)}
\usage{
DEMA(x, n = 10, v = 1, wilder = FALSE, ratio = NULL)
}
\arguments{
\item{x}{Price series that is coercible to xts or matrix.}

\item{n}{Number of periods to average over. Must be between 1 and \code{nrow(x)}.}

\item{v}{The 'volume factor' (a number in \code{[0,1\]}; default is 1 (standard DEMA).}

\item{wilder}{logical; if \code{TRUE}, use Welles Wilder's EMA formula.}

\item{ratio}{A smoothing/decay ratio (overrides \code{wilder}).}
}
\value{
An object of the same class as \code{x} containing the DEMA values.
}
\description{
Calculate a double exponential moving average to reduce lag.
}
\note{
When \code{v=1}, returns standard DEMA; \code{v=0} returns regular EMA.
}
\examples{
data(TSLA)
dema_20 <- DEMA(TSLA[, "Close"], 20)
head(dema_20)
}
\keyword{ts}
