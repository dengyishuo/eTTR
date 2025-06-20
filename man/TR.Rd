% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TR.R
\name{TR}
\alias{TR}
\title{True Range}
\usage{
TR(HLC)
}
\arguments{
\item{HLC}{Object that is coercible to xts or matrix and contains High-Low-Close prices.}
}
\value{
A object of the same class as \code{HLC} or a matrix (if \code{try.xts} fails)
containing the columns:
\describe{
\item{tr}{The true range of the series.}
\item{trueHigh}{The true high used in calculating the true range.}
\item{trueLow}{The true low used in calculating the true range.}
}
}
\description{
Calculates the True Range of a price series.
}
\examples{
data(TSLA)
tr <- TR(TSLA[, c("High", "Low", "Close")])
}
\seealso{
\code{\link{ATR}} for Average True Range.
}
\author{
DengYishuo
}
\keyword{ts}
