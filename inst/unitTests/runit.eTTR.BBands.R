# RUnit tests for Bollinger Bands (BBands)
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

# BBands test suite
test.BBands <- function() {
  suite <- defineTestSuite("BBands",
    testfunctions = list(
      test.BBands
    ),
    dir = "tests"
  )
  return(suite)
}

test.BBands <- function() {
  ia <- input$all[, c("High", "Low", "Close")]
  it <- input$top[, c("High", "Low", "Close")]
  im <- input$mid[, c("High", "Low")]
  rownames(ia) <- rownames(it) <- NULL
  oa <- BBands(ia)
  ot <- BBands(it)
  rownames(oa) <- rownames(ot) <- rownames(input$all)

  checkEqualsNumeric(oa, output$allBBands, "BBands calculation error")
  checkEquals(attributes(oa), attributes(output$allBBands), "BBands attributes mismatch")
  checkEqualsNumeric(ot, output$topBBands, "BBands with NA input error")
  checkEquals(attributes(ot), attributes(output$topBBands), "BBands attributes with NA mismatch")
  checkException(BBands(im), "BBands should throw error on incomplete input")
}
