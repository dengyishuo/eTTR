# RUnit tests for Rogers-Satchell volatility with non-leading NA check
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

# Rogers-Satchell test suite - Fixed defineTestSuite call
test.RogersSatchell <- function() {
  suite <- defineTestSuite(
    name = "Rogers_Satchell",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test Rogers-Satchell volatility calculation
test.rogers.satchell <- function() {
  ohlc <- c("Open", "High", "Low", "Close")

  # Test with valid data (no leading NA)
  rs_all <- volatility(input$all[, ohlc], calc = "rogers.satchell")
  checkEqualsNumeric(rs_all[["x"]], output$allRS[["x"]], "Rogers-Satchell calculation error")

  # Test with leading NA (allowed)
  rs_top <- volatility(input$top[, ohlc], calc = "rogers.satchell")
  checkEqualsNumeric(rs_top[["x"]], output$topRS[["x"]], "Rogers-Satchell with leading NA error")

  # Test with non-leading NA (should throw error)
  checkException(
    volatility(input$mid[, ohlc], calc = "rogers.satchell"),
    "Rogers-Satchell should throw error on non-leading NA"
  )
}
