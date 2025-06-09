# RUnit tests for Relative Strength Index (RSI)
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

# RSI test suite
test.RSI <- function() {
  suite <- defineTestSuite("RSI",
    testfunctions = list(
      test.RSI,
      test.RSI.does.not.overwrite.maArgs
    ),
    dir = "tests"
  )
  return(suite)
}

test.RSI <- function() {
  checkEqualsNumeric(RSI(input$all$Close), output$allRSI, "RSI calculation error")
  checkEquals(attributes(RSI(input$all$Close)), attributes(output$allRSI), "RSI attributes mismatch")
  checkEqualsNumeric(RSI(input$top$Close), output$topRSI, "RSI with NA input error")
  checkEquals(attributes(RSI(input$top$Close)), attributes(output$topRSI), "RSI attributes with NA mismatch")
  checkException(RSI(input$mid$Close), "RSI should throw error on partial NA")
}

test.RSI.does.not.overwrite.maArgs <- function() {
  wilder.and.matype <- RSI(input$all$Close, maType = "EMA", wilder = FALSE)
  wilder.only <- RSI(input$all$Close, wilder = FALSE)
  checkEqualsNumeric(wilder.and.matype, wilder.only, "RSI maArgs overwrite error")
}
