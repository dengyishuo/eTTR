### About eTTR

eTTR is an R package that provides the most popular technical analysis functions
for financial market data. Many of these functions are used as components of
systematic trading strategies and financial charts. The package also enables
convenient generation of trading signals via `gen_Sig` function. Additionally,
the `ebacktest` function allows for lightweight backtesting of trading signals,
returning all investment performance metrics during becktest periods.
Furthermore, it will enrich the visualization of technical indicators in the
future, making it easier to interpret and analyze market trends through intuitive
graphical representations.

### Installation eTTR

The current release is not available on CRAN. To install the development version,
you need to clone the repository and build from source, or run one of:

```r
# lightweight
remotes::install_github("dengyishuo/eTTR")
# or
devtools::install_github("dengyishuo/eTTR")
```

You will need tools to compile C/C++ code. See the relevant
appendix in the [R Installation and Administration manual](https://cran.r-project.org/doc/manuals/r-release/R-admin.html)
for your operating system:

- [Windows](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#The-Windows-toolset)
- [MacOS](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#macOS) (the [R for Mac OS X Developer's Page](https://mac.R-project.org/) might also be helpful)
- [Unix-alike](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Essential-and-useful-other-programs-under-a-Unix_002dalike)

### Getting Started

Here are a few examples of some of the more well-known indicators:

```r
# "eTTR TSLA"
data(TSLA)
hlc <- TSLA[, c("High", "Low", "Close")]

# Bollinger Bands
bbands <- BBands(hlc)

# Directional Movement Index
adx <- ADX(hlc)

# Moving Averages
ema <- EMA(TSLA[, "Close"], n = 20)
sma <- SMA(TSLA[, "Close"], n = 20)

# MACD
macd <- MACD(TSLA[,"Close"])

# RSI
rsi <- RSI(TSLA[,"Close"])

# Stochastics
stochOsc <- stoch(hlc)
```

eTTR works with the `chartSeries()` function in [quantmod](https://github.com/joshuaulrich/quantmod). Here's an example that uses `chartSeries()` and adds TTR-calculated indicators and overlays to the chart.

```r
library(quantmod)
data(TSLA)


chartSeries(TSLA, subset = "2020-09/", theme = "white")
addBBands()
addRSI()
```

![ex1](img/ex1.png "eTTR example_1")


### Have a question?

Ask your question on [Stack Overflow](https://stackoverflow.com/questions/tagged/r)
or the [R-SIG-Finance](https://stat.ethz.ch/mailman/listinfo/r-sig-finance)
mailing list (you must subscribe to post).


### See Also

- [quantmod](https://CRAN.R-project.org/package=quantmod): quantitative financial modeling framework
- [xts](https://CRAN.R-project.org/package=xts): eXtensible Time Series based
on [zoo](https://CRAN.R-project.org/package=zoo)

### Author

[DengYishuo](https://gewutang.com/about/)
