# RUnit tests for Williams' Percent R (WPR)
library(RUnit)

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list(all = ttrc[1:250, ], top = ttrc[1:250, ], mid = ttrc[1:250, ])
input$top[1:10, ] <- NA
input$mid[9:20, ] <- NA

# Load output data
load(system.file("unitTests/output.Oscillators.rda", package = "eTTR"))

# WPR test suite
test.WPR <- function() {
  suite <- defineTestSuite("WPR",
    testfunctions = list(
      test.WPR
    ),
    dir = "tests"
  )
  return(suite)
}

test.WPR <- function() {
  # Handle data frame to matrix conversion for attributes
  ia <- input$all[, c("High", "Low", "Close")]
  it <- input$top[, c("High", "Low", "Close")]
  rn <- rownames(ia)
  rownames(ia) <- rownames(it) <- NULL
  oa <- WPR(ia)
  names(oa) <- rn
  ot <- WPR(it)
  names(ot) <- rn
  checkEqualsNumeric(oa, output$allWPR, "WPR calculation error")
  checkEquals(attributes(oa), attributes(output$allWPR), "WPR attributes mismatch")
  checkEqualsNumeric(ot, output$topWPR, "WPR with NA input error")
  checkEquals(attributes(ot), attributes(output$topWPR), "WPR attributes with NA mismatch")
  checkException(WPR(input$mid$Close), "WPR should throw error on invalid input")
}
