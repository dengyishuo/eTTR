# RUnit tests for TRIX oscillator
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

# TRIX test suite
test.TRIX <- function() {
  suite <- defineTestSuite("TRIX",
    testfunctions = list(test.TRIX),
    dir = "tests"
  )
  return(suite)
}

test.TRIX <- function() {
  checkEqualsNumeric(TRIX(input$all$Close), output$allTRIX, "TRIX calculation error")
  checkEquals(attributes(TRIX(input$all$Close)), attributes(output$allTRIX), "TRIX attributes mismatch")
  checkEqualsNumeric(TRIX(input$top$Close), output$topTRIX, "TRIX with NA input error")
  checkEquals(attributes(TRIX(input$top$Close)), attributes(output$topTRIX), "TRIX attributes with NA mismatch")
  checkException(TRIX(input$mid$Close), "TRIX should throw error on partial NA")
}
