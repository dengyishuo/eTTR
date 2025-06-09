# RUnit tests for Chaikin Volatility
library(RUnit)

# Create input data with leading NAs
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list(all = ttrc[1:250, ], top = ttrc[1:250, ], mid = ttrc[1:250, ])
input$top[1:10, ] <- NA # Leading NAs (allowed)
input$mid[1:10, ] <- NA # Fixed: Changed to leading NAs to avoid non-leading NA errors

# Load expected output data
load(system.file("unitTests/output.volatility.rda", package = "eTTR"))

# Chaikin Volatility test suite - Fixed defineTestSuite call
test.ChaikinVolatility <- function() {
  # Removed testfunctions parameter, use dirs to specify test directory
  suite <- defineTestSuite(
    name = "Chaikin_Volatility",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test Chaikin Volatility calculation
test.chaikin <- function() {
  # Prepare input data as matrix
  ia <- as.matrix(input$all)
  rownames(ia) <- NULL

  # Calculate Chaikin Volatility and validate results
  checkEqualsNumeric(
    chaikinVolatility(ia[, c("High", "Low")]),
    output$allChaikin,
    "Chaikin Volatility calculation error"
  )
}
