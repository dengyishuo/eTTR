% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runFun.R
\name{runSum}
\alias{runSum}
\alias{runFun}
\alias{runMin}
\alias{runMax}
\alias{runMean}
\alias{runMedian}
\alias{runCov}
\alias{runCor}
\alias{runVar}
\alias{runSD}
\alias{runMAD}
\alias{wilderSum}
\title{Analysis of Running/Rolling/Moving Windows}
\usage{
runSum(x, n = 10, cumulative = FALSE)

runMin(x, n = 10, cumulative = FALSE)

runMax(x, n = 10, cumulative = FALSE)

runMean(x, n = 10, cumulative = FALSE)

runMedian(x, n = 10, non.unique = "mean", cumulative = FALSE)

runCov(x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE)

runCor(x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE)

runVar(x, y = NULL, n = 10, sample = TRUE, cumulative = FALSE)

runSD(x, n = 10, sample = TRUE, cumulative = FALSE)

runMAD(
  x,
  n = 10,
  center = NULL,
  stat = "median",
  constant = 1.4826,
  non.unique = "mean",
  cumulative = FALSE
)

wilderSum(x, n = 10)
}
\arguments{
\item{x}{Object coercible to xts or matrix.}

\item{n}{Number of periods to use in the window or, if
\code{cumulative=TRUE}, the number of observations to use before the first
result is returned. Must be between 1 and \code{nrow(x)}, inclusive.}

\item{cumulative}{Logical, use from-inception calculation?}

\item{non.unique}{One of 'mean', 'max', or 'min'; which compute their
respective statistics for the two middle values of even-sized samples.}

\item{y}{Object coercible to xts or matrix.}

\item{use}{Only \code{"all.obs"} currently implemented.}

\item{sample}{Logical, sample covariance if \code{TRUE} (denominator of
\code{n-1})}

\item{center}{The values to use as the measure of central tendency, around
which to calculate deviations. The default (\code{NULL}) uses the median.}

\item{stat}{Statistic to calculate, one of 'median' or 'mean' (e.g. median
absolute deviation or mean absolute deviation, respectively.)}

\item{constant}{Scale factor applied to approximate the standard deviation.}
}
\value{
A object of the same class as \code{x} and \code{y} or a vector (if
\code{try.xts} fails).
 \describe{
  \item{runSum}{returns sums over a n-period moving window.}
  \item{runMin}{returns minimums over a n-period moving window.}
  \item{runMax}{returns maximums over a n-period moving window.}
  \item{runMean}{returns means over a n-period moving window.}
  \item{runMedian}{returns medians over a n-period moving window.}
  \item{runCov}{returns covariances over a n-period moving window.}
  \item{runCor}{returns correlations over a n-period moving window.}
  \item{runVar}{returns variances over a n-period moving window.}
  \item{runSD}{returns standard deviations over a n-period moving window.}
  \item{runMAD}{returns median/mean absolute deviations over a n-period moving window.}
  \item{wilderSum}{retuns a Welles Wilder style weighted sum over a n-period moving window.}
 }
}
\description{
Various functions to analyze data over a moving window of periods.
}
\author{
DengYishuo
}
\keyword{ts}
