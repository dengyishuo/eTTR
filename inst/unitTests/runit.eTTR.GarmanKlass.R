# RUnit tests for Garman-Klass volatility with non-leading NA check
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

# Garman-Klass test suite - Fixed defineTestSuite call
test.GarmanKlass <- function() {
  suite <- defineTestSuite(
    name = "Garman_Klass",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test Garman-Klass volatility calculation
test.garman.klass <- function() {
  ohlc <- c("Open", "High", "Low", "Close")

  # Test with valid data (no leading NA)
  gk_all <- volatility(input$all[, ohlc], calc = "garman.klass")
  checkEqualsNumeric(gk_all[["x"]], output$allGK[["x"]], "Garman-Klass calculation error")

  # Test with leading NA (allowed)
  gk_top <- volatility(input$top[, ohlc], calc = "garman.klass")
  checkEqualsNumeric(gk_top[["x"]], output$topGK[["x"]], "Garman-Klass with leading NA error")

  # Test with non-leading NA (should throw error)
  checkException(
    volatility(input$mid[, ohlc], calc = "garman.klass"),
    "Garman-Klass should throw error on non-leading NA"
  )
}
