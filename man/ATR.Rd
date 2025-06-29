% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ATR.R
\name{ATR}
\alias{ATR}
\title{Calculate Average True Range (ATR)}
\usage{
ATR(HLC, n = 14, maType, ...)
}
\arguments{
\item{HLC}{An OHLC (Open-High-Low-Close) price series, typically an xts object.}

\item{n}{The number of periods to use for the moving average calculation. Defaults to 14.}

\item{maType}{The type of moving average to use. Defaults to "EMA" (Wilder's EMA).}

\item{...}{Additional arguments to be passed to the moving average function.}
}
\value{
An xts object containing columns for the true range ("tr"), the ATR ("atr"),
and the true high and low values ("trueHigh", "trueLow").
}
\description{
This function computes the Average True Range (ATR), a technical analysis indicator
used to measure market volatility. It calculates the true range (TR) and applies a moving
average (defaulting to Wilder's EMA) to smooth the results.
}
\details{
The Average True Range (ATR) is a measure of volatility introduced by J. Welles Wilder.
It considers the entire range of price movement in a given period, including gaps between
sessions. The default method uses Wilder's EMA, which applies a smoothing factor of 1/n.
}
\examples{
\dontrun{
# Calculate ATR for a stock's OHLC data
data(sample_matrix)
stock_ohlc <- as.xts(sample_matrix)
atr_values <- ATR(stock_ohlc, n = 14)
}
}
\references{
Wilder, J. Welles. "New Concepts in Technical Trading Systems." 1978.
}
\author{
DengYishuo
}
