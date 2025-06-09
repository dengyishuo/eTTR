# RUnit tests for ZigZag indicator
library(RUnit)

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list(all = ttrc[1:250, ], top = ttrc[1:250, ], mid = ttrc[1:250, ])
input$top[1:10, ] <- NA
input$mid[9:20, ] <- NA

# Load output data
load(system.file("unitTests/output.overlays.rda", package = "eTTR"))

# ZigZag test suite
test.ZigZag <- function() {
  suite <- defineTestSuite("ZigZag",
    testfunctions = list(
      test.ZigZag
    ),
    dir = "tests"
  )
  return(suite)
}

test.ZigZag <- function() {
  ia <- input$all[, c("High", "Low")]
  it <- input$top[, c("High", "Low")]
  im <- input$mid[, c("High", "Low")]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL

  checkEqualsNumeric(ZigZag(ia), output$allZZ, "ZigZag calculation error")
  checkEquals(attributes(ZigZag(ia)), attributes(output$allZZ), "ZigZag attributes mismatch")
  checkEqualsNumeric(ZigZag(it), output$topZZ, "ZigZag with NA input error")
  checkEquals(attributes(ZigZag(it)), attributes(output$topZZ), "ZigZag attributes with NA mismatch")
  checkException(ZigZag(im), "ZigZag should throw error on partial NA input")
}
