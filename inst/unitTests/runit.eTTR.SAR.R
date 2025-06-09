# RUnit tests for SAR (Stop and Reverse)
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

# SAR test suite
test.SAR <- function() {
  suite <- defineTestSuite("SAR",
    testfunctions = list(
      test.SAR
    ),
    dir = "tests"
  )
  return(suite)
}

test.SAR <- function() {
  ia <- input$all[, c("High", "Low")]
  it <- input$top[, c("High", "Low")]
  im <- input$mid[, c("High", "Low")]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL

  checkEqualsNumeric(SAR(ia), output$allSAR, tolerance = 0.5, "SAR calculation error")
  checkEquals(attributes(SAR(ia)), attributes(output$allSAR), "SAR attributes mismatch")
  checkEqualsNumeric(SAR(it), output$topSAR, "SAR with NA input error")
  checkEquals(attributes(SAR(it)), attributes(output$topSAR), "SAR attributes with NA mismatch")
  checkException(SAR(im), "SAR should throw error on partial NA input")
}
