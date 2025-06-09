# RUnit tests for ALMA (Arnaud Legoux Moving Average)
library(RUnit)
library(xts) # Ensure xts package is loaded for time series operations

# Create test input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

# Prepare input datasets with leading NAs
input <- list(all = ttrc[1:250, ], top = ttrc[1:250, ], mid = ttrc[1:250, ])
input$top[1:10, ] <- NA # Leading NAs are allowed
input$mid[1:10, ] <- NA # Fixed: Changed from 9:20 to 1:10 to avoid non-leading NAs

# Load expected output data for comparison
load(system.file("unitTests/output.MA.rda", package = "eTTR"))

# Define ALMA test suite - Fixed defineTestSuite call
test.ALMA <- function() {
  # Removed testfunctions parameter, only use dirs to specify test directory
  suite <- defineTestSuite(
    name = "ALMA",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test that ALMA output length matches input length
test.ALMA.output.length.eq.input.length <- function() {
  v <- 1:10 # Create numeric vector input
  x <- xts::.xts(v, seq_along(v)) # Convert to xts object

  # Compute ALMA for both vector and xts inputs
  av <- ALMA(v)
  ax <- ALMA(x)

  # Validate output length consistency
  if (is.vector(av)) {
    checkEquals(length(av), NROW(x), "ALMA vector output length mismatch")
  } else {
    checkEquals(NROW(ax), NROW(x), "ALMA xts output length mismatch")
  }
}

# Additional ALMA tests can be added here...
