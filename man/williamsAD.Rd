% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/williamsAD.R
\name{williamsAD}
\alias{williamsAD}
\title{Williams Accumulation / Distribution}
\usage{
williamsAD(HLC)
}
\arguments{
\item{HLC}{Object that is coercible to xts or matrix and contains
High-Low-Close prices.}
}
\value{
A object of the same class as \code{HLC} or a vector (if
\code{try.xts} fails) containing the accumulation / distribution values.
}
\description{
The Williams Accumulation / Distribution (AD) line is a measure of market
momentum.  Developed by Larry Williams.

The Williams AD line differs from OBV and chaikinAD in that it doesn't take
volume into account.
}
\note{
The Accumulation/Distribution Line is interpreted by looking for a
divergence in the direction of the indicator relative to price.
}
\examples{
data(TSLA)
ad <- williamsAD(TSLA[, c("High", "Low", "Close")])
}
\references{
The following site(s) were used to code/document this
indicator:\cr
\url{https://www.fmlabs.com/reference/WilliamsAD.htm}\cr
\url{https://www.metastock.com/Customer/Resources/TAAZ/?p=125}\cr
}
\seealso{
See \code{\link{OBV}}, \code{\link{chaikinAD}}, and
\code{\link{ATR}}.
}
\author{
DengYishuo
}
\keyword{ts}
