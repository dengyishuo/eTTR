% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_TR.R
\name{add_TR}
\alias{add_TR}
\title{Calculate True Range}
\usage{
add_TR(OHLCV, append = FALSE)
}
\arguments{
\item{OHLCV}{Object that is coercible to xts or matrix, assumed to contain Open - High - Low - Close - Volume data.}

\item{append}{A logical value. If \code{TRUE}, the calculated True Range, True High, and True Low
values will be appended to the \code{OHLCV} input data, ensuring
proper alignment of time - series data. If \code{FALSE}, only the calculated
True Range, True High, and True Low values will be returned. Defaults to \code{FALSE}.}
}
\value{
If \code{append = FALSE}, an object of the same class as \code{OHLCV}
or a matrix (if \code{try.xts} fails) containing the columns:
\describe{
\item{tr}{The true range of the series.}
\item{trueHigh}{The true high used in calculating the true range.}
\item{trueLow}{The true low used in calculating the true range.}
}
If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
calculated True Range, True High, and True Low values appended, maintaining the integrity of the time - series
alignment.
}
\description{
Calculates the True Range of a price series.
}
\examples{
\dontrun{
data(TSLA)
# Using default parameters without appending
tr_result1 <- add_TR(TSLA)

# Using default parameters and appending
tr_result2 <- add_TR(TSLA, append = TRUE)
}
}
\seealso{
\code{\link{ATR}} for Average True Range.
}
\author{
DengYishuo
}
\keyword{ts}
