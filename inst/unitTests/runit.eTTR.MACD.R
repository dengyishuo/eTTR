# RUnit tests for MACD oscillator
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

# MACD test suite
test.MACD <- function() {
  suite <- defineTestSuite("MACD",
    testfunctions = list(
      test.MACD
    ),
    dir = "tests"
  )
  return(suite)
}

test.MACD <- function() {
  checkEqualsNumeric(MACD(input$all$Close), output$allMACD, "MACD calculation error")
  checkEquals(attributes(MACD(input$all$Close)), attributes(output$allMACD), "MACD attributes mismatch")
  checkEqualsNumeric(MACD(input$top$Close), output$topMACD, "MACD with NA input error")
  checkEquals(attributes(MACD(input$top$Close)), attributes(output$topMACD), "MACD attributes with NA mismatch")
  checkException(MACD(input$mid$Close), "MACD should throw error on partial NA")
}
