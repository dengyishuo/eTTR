# RUnit tests for Simple Moving Average (SMA)
library(RUnit)

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list(all = ttrc[1:250, ], top = ttrc[1:250, ], mid = ttrc[1:250, ])
input$top[1:10, ] <- NA
input$mid[9:20, ] <- NA

# Load output data
load(system.file("unitTests/output.MA.rda", package = "eTTR"))

# SMA test suite
test.SMA <- function() {
  suite <- defineTestSuite("SMA",
    testfunctions = list(
      test.SMA
    ),
    dir = "tests"
  )
  return(suite)
}

test.SMA <- function() {
  checkEqualsNumeric(SMA(input$all$Close), output$allSMA, "SMA calculation error")
  checkEquals(attributes(SMA(input$all$Close)), attributes(output$allSMA), "SMA attributes mismatch")
  checkEqualsNumeric(SMA(input$top$Close), output$topSMA, "SMA with NA input error")
  checkEquals(attributes(SMA(input$top$Close)), attributes(output$topSMA), "SMA attributes with NA mismatch")
  checkException(SMA(input$mid$Close), "SMA should throw error on partial NA")
  checkException(SMA(input$all[, 1:2]), "SMA should reject multi-column input")
}
