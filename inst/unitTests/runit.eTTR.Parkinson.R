# RUnit tests for Parkinson volatility with non-leading NA check
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
input$mid[9:20, ] <- NA # Non-leading NAs (triggers error)

# Load expected output data
load(system.file("unitTests/output.volatility.rda", package = "eTTR"))

# Parkinson test suite - Fixed defineTestSuite call
test.Parkinson <- function() {
  suite <- defineTestSuite(
    name = "Parkinson",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test Parkinson volatility calculation
test.parkinson <- function() {
  ohlc <- c("Open", "High", "Low", "Close")

  # Test with valid data (no leading NA)
  pk_all <- volatility(input$all[, ohlc], calc = "parkinson")
  checkEqualsNumeric(pk_all[["x"]], output$allParkinson[["x"]], "Parkinson calculation error")

  # Test with leading NA (allowed)
  pk_top <- volatility(input$top[, ohlc], calc = "parkinson")
  checkEqualsNumeric(pk_top[["x"]], output$topParkinson[["x"]], "Parkinson with leading NA error")

  # Test with non-leading NA (should throw error)
  checkException(
    volatility(input$mid[, ohlc], calc = "parkinson"),
    "Parkinson should throw error on non-leading NA"
  )
}
