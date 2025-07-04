% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runMAD.R
\name{runMAD}
\alias{runMAD}
\title{Moving Window Median Absolute Deviation (runMAD)}
\usage{
runMAD(
  x,
  n = 10,
  center = NULL,
  stat = "median",
  constant = 1.4826,
  non.unique = "mean",
  cumulative = FALSE
)
}
\arguments{
\item{x}{Object coercible to xts or matrix.}

\item{n}{Number of periods in the window (1 <= n <= nrow(x)).}

\item{center}{Central tendency to use (NULL for median).}

\item{stat}{Type of MAD: 'median' (default) or 'mean'.}

\item{constant}{Scale factor for median MAD (default 1.4826).}

\item{non.unique}{Handling of even-sized windows: 'mean', 'max', or 'min'.}

\item{cumulative}{Logical; if TRUE, use from-inception calculation.}
}
\value{
An object of the same class as \code{x} with MAD values.
}
\description{
Calculate the median absolute deviation over a moving window of periods.
}
\examples{
data(TSLA)
mad_20 <- runMAD(TSLA[, "Close"], 20)
head(mad_20)
}
\keyword{internal}
\keyword{ts}
