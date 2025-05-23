% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WebData.R
\name{stockSymbols}
\alias{stockSymbols}
\alias{WebData}
\alias{getYahooData}
\title{Fetch Internet Data}
\usage{
stockSymbols(
  exchange = c("AMEX", "NASDAQ", "NYSE", "ARCA", "BATS", "IEX"),
  sort.by = c("Exchange", "Symbol"),
  quiet = FALSE
)

getYahooData(
  symbol,
  start,
  end,
  freq = "daily",
  type = "price",
  adjust = TRUE,
  quiet = FALSE
)
}
\arguments{
\item{exchange}{Character vector of exchange names on which desired
instrument symbols are traded.}

\item{sort.by}{Character vector of columns by which returned data will be
sorted.  Must be one or more of \code{"Name"}, \code{"Symbol"},
\code{"Market.Cap"}, or \code{"Exchange"}.}

\item{quiet}{Logical; if \code{TRUE}, status messages will be printed to the
console.}

\item{symbol}{Yahoo! Finance instrument symbol.}

\item{start}{Numeric; first date of desired data, in YYYYMMDD format.
Default is first date of series.}

\item{end}{Numeric; last date of desired data, in YYYYMMDD format.  Default
is last date of series.}

\item{freq}{Desired data frequency.  One of \code{"daily"}, \code{"weekly"},
\code{"monthly"}.}

\item{type}{Type of data to return.  One of \code{"price"}, or
\code{"split"}.  \code{type="split"} will return both split and dividend
data.}

\item{adjust}{Logical; if \code{TRUE}, the Open, High, Low, and Close prices
will be adjusted for dividends and splits, and Volume will be adjusted for
dividends.}
}
\value{
\code{getYahooData} returns an xts object containing the columns:

\code{stockSymbols} returns a character vector containing all the listed
symbols for the given exchanges.
 \describe{
    \item{ Date }{ Trade date, in CCYYMMDD format. }
    \item{ Open }{ Open price. }
    \item{ High }{ High price. }
    \item{ Low }{ Low price. }
    \item{ Close }{ Close price. }
    \item{ Volume }{ Volume. }
 }
}
\description{
Get investment data from the internet.
}
\details{
\code{getYahooData} fetches individual stock data from the Yahoo! Finance
website.  It also adjusts price for splits and dividends, and volume for
splits.  See the Warning section, and note that it is deprecated in favor
of getSymbols in the quantmod package.

\code{stockSymbols} fetches instrument symbols from the nasdaq.com website,
and adjusts the symbols to be compatible with the Yahoo! Finance website.
}
\note{
The symbols returned by \code{stockSymbols} may not be in the format
necessary to retrieve data using \code{getYahooData}.

\code{getYahooData} has only been tested on daily data.  It isn't known if
the function correctly adjusts data for any other frequency.
}
\section{Warning}{

As of TTR 0.23-2, \code{getYahooData} has been patched to work with changes
to Yahoo Finance, which also included the following changes to the raw data:
  \itemize{
    \item The adjusted close column appears to no longer include dividend adjustments
    \item The open, high, and low columns are adjusted for splits, and
    \item The raw data may contain missing values.
    \item The raw data may contain errors.
  }

As of TTR 0.24.2, \code{stockSymbols} began using data from NASDAQ's FTP
site because the data from the original site is no longer available. This
new file does not contain data for the columns: LastSale, MarketCap,
IPOyear, Sector, and Industry. All the columns still appear in the results,#' but all the values in the columns are set to \code{NA}.
}

\examples{

 ### Note: you must have a working internet
 ### connection for these examples to work!
 if (interactive()) {
   ge <- getYahooData("GE", 19990404, 20050607, adjust = FALSE)

   nyse.symbols <- stockSymbols("NYSE")
 }

}
\references{
\itemize{
   \item \href{https://quant.stackexchange.com/questions/1640/where-to-download-list-of-all-common-stocks-traded-on-nyse-nasdaq-and-amex/1862}{Quant StackExchange: Download list of all stock symbols?}
   \item \href{https://www.nasdaqtrader.com/trader.aspx?id=CQSsymbolconvention}{CQS symbol convention}
   \item \href{https://web.archive.org/web/20111023221931/http://help.yahoo.com/l/us/yahoo/finance/quotes/quote-02.html}{Yahoo Finance symbol conventions}
 }
}
\author{
DengYishuo
}
\keyword{ts}
