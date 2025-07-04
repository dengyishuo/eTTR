% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_adx.R
\name{add_adx}
\alias{add_adx}
\alias{add_ADX}
\title{Welles Wilder's Directional Movement Index}
\usage{
add_adx(OHLCV, n = 14, maType, append = FALSE, ...)
}
\arguments{
\item{OHLCV}{Object that is coercible to \code{xts} or \code{matrix} and contains
Open - High - Low - Close - Volume (and potentially other) prices. It serves as
the input data source for the calculations.}

\item{n}{Number of periods to use for DX calculation (not ADX calculation).
Defaults to 14.}

\item{maType}{A function or a string naming the function to be called. This is
used to specify the type of moving average for calculating the ADX.}

\item{append}{A logical value. If \code{TRUE}, the calculated result columns
(DIp, DIn, DX, ADX) will be appended to the \code{OHLCV} input data, ensuring
proper alignment of time - series data. If \code{FALSE}, only the calculated
result data will be returned. Defaults to \code{FALSE}}

\item{...}{Other arguments to be passed to the \code{maType} function.}
}
\value{
If \code{append = FALSE}, an object of the same class as \code{OHLCV}
(or a matrix if \code{try.xts} fails) containing the columns:
\describe{
\item{DIp}{The positive Direction Index.}
\item{DIn}{The negative Direction Index.}
\item{DX}{The Direction Index.}
\item{ADX}{The Average Direction Index (trend strength).}
}
If \code{append = TRUE}, an object of the same class as \code{OHLCV} with the
calculated columns appended, maintaining the integrity of the time - series
alignment.
}
\description{
Directional Movement Index; developed by J. Welles Wilder.
The \code{DIp}/\code{DIn} (positive/negative) is the percentage of the true
range that is up/down. This function calculates the Directional Movement Index
and related components.
}
\note{
A buy/sell signal is generated when the +/-DI crosses up over the
-/+DI, when the DX/ADX signals a strong trend. A high/low DX signals a
strong/weak trend. DX is usually smoothed with a moving average (i.e. the
ADX).
}
\examples{
\dontrun{
# Load required data, assume TSLA is data in OHLCV format
data(TSLA)

# Use default parameters
dmi.adx1 <- add_adx(TSLA)

# Modify the n parameter
dmi.adx2 <- add_adx(TSLA, n = 20)

# Use SMA as maType
dmi.adx3 <- add_adx(TSLA, maType = "SMA")

# Use SMA and pass additional parameters to the SMA function
dmi.adx4 <- add_adx(TSLA, maType = "SMA")

# Set append to TRUE
dmi.adx5 <- add_adx(TSLA, append = TRUE)

# Combine modifying the n parameter and setting append to TRUE
dmi.adx6 <- add_adx(TSLA, n = 20, append = TRUE)

# Combine modifying the n parameter, using SMA as maType, and setting append to TRUE
dmi.adx7 <- add_adx(TSLA, n = 20, maType = "SMA", append = TRUE)
}
}
\references{
The following site(s) were used to code/document this
indicator:\cr \url{https://www.fmlabs.com/reference/DI.htm}\cr
\url{https://www.fmlabs.com/reference/DX.htm}\cr
\url{https://www.fmlabs.com/reference/ADX.htm}\cr
\url{https://www.fmlabs.com/reference/ADXR.htm}\cr
\url{https://www.metastock.com/Customer/Resources/TAAZ/?p=49}\cr
\url{https://www.linnsoft.com/techind/directional-indicator-diplus-diminus}\cr
\url{https://www.linnsoft.com/techind/adx-avg-directional-movement}\cr
\url{https://www.linnsoft.com/techind/adxr-avg-directional-movement-rating}\cr
\url{https://school.stockcharts.com/doku.php?id=technical_indicators:average_directional_index_adx}\cr
}
\seealso{
See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
options; and note Warning section. The DX calculation uses
\code{\link{ATR}}. See \code{\link{aroon}}, \code{\link{CCI}},
\code{\link{TDI}}, \code{\link{VHF}}, \code{\link{GMMA}} for other indicators
that measure trend direction/strength.
}
\author{
DengYishuo
}
\keyword{ts}
