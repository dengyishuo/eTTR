% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ATR.R
\name{TR}
\alias{TR}
\alias{ATR}
\title{True Range / Average True Range}
\usage{
TR(HLC)

ATR(HLC, n = 14, maType, ...)
}
\arguments{
\item{HLC}{Object that is coercible to xts or matrix and contains
High-Low-Close prices.}

\item{n}{Number of periods for moving average.}

\item{maType}{A function or a string naming the function to be called.}

\item{\dots}{Other arguments to be passed to the \code{maType} function.}
}
\value{
A object of the same class as \code{HLC} or a matrix (if
\code{try.xts} fails) containing the columns:
 \describe{
  \item{ tr }{ The true range of the series. }
  \item{ atr }{ The average (as specified by \code{ma}) true range of the series. }
  \item{ trueHigh }{ The true high of the series. }
  \item{ trueLow }{ The true low of the series. }
 }
}
\description{
True range (TR) is a measure of volatility of a High-Low-Close series;
average true range (ATR) is a Welles Wilder's style moving average of the TR.
Developed by J. Welles Wilder in 1978.
}
\details{
TR incorporates yesterday's close in the calculation (high minus low).  E.g.
if yesterday's close was higher than today's high, then the TR would equal
yesterday's close minus today's low.

The ATR is a component of the Welles Wilder Directional Movement Index
(\code{DX}, \code{ADX}).
}
\examples{

data(ttrc)
tr <- TR(ttrc[, c("High", "Low", "Close")])
atr <- ATR(ttrc[, c("High", "Low", "Close")], n = 14)

}
\references{
The following site(s) were used to code/document this
indicator:\cr \url{https://www.fmlabs.com/reference/TR.htm}\cr
\url{https://www.fmlabs.com/reference/ATR.htm}\cr
\url{https://www.metastock.com/Customer/Resources/TAAZ/?p=35}\cr
\url{https://www.linnsoft.com/techind/true-range-tr}\cr
\url{https://school.stockcharts.com/doku.php?id=technical_indicators:average_true_range_atr}\cr
}
\seealso{
See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
options; and note Warning section.  See \code{\link{DX}}, which uses true
range.  See \code{\link{chaikinVolatility}} for another volatility measure.
}
\author{
DengYishuo
}
\keyword{ts}
