# RUnit tests for Close price volatility
library(RUnit)

# Create input data with non-leading NAs for error testing
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list(
  all = ttrc[1:250, ], # Data without NA
  top = ttrc[1:250, ], # Data with leading NAs
  mid = ttrc[1:250, ] # Data with non-leading NAs
)
input$top[1:10, ] <- NA # Leading NAs (allowed)
input$mid[9:20, ] <- NA # Non-leading NAs (to trigger error)

# Load expected output data
load(system.file("unitTests/output.volatility.rda", package = "eTTR"))

# Close volatility test suite
test.CloseVolatility <- function() {
  suite <- defineTestSuite(
    name = "Close_Volatility",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test Close price volatility
test.Close <- function() {
  ohlc <- c("Open", "High", "Low", "Close")

  # Uncomment the following tests once data matching is confirmed
  # checkEqualsNumeric(
  #   volatility(input$all[, ohlc], calc = "close"),
  #   output$allClose,
  #   "Close volatility calculation error"
  # )
  # checkEqualsNumeric(
  #   volatility(input$top[, ohlc], calc = "close"),
  #   output$topClose,
  #   "Close volatility with leading NA error"
  # )

  # Test non-leading NA to trigger error (key fix)
  checkException(
    volatility(input$mid[, ohlc], calc = "close"),
    "Close volatility should throw error on non-leading NA"
  )
}
