# RUnit tests for De-trended Price Oscillator (DPO)
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

# DPO test suite
test.DPO <- function() {
  suite <- defineTestSuite("DPO",
    testfunctions = list(
      test.DPO
    ),
    dir = "tests"
  )
  return(suite)
}

test.DPO <- function() {
  checkEqualsNumeric(DPO(input$all$Close), output$allDPO, "DPO calculation error")
  checkEquals(attributes(DPO(input$all$Close)), attributes(output$allDPO), "DPO attributes mismatch")
  checkEqualsNumeric(DPO(input$top$Close), output$topDPO, "DPO with NA input error")
  checkEquals(attributes(DPO(input$top$Close)), attributes(output$topDPO), "DPO attributes with NA mismatch")
  checkException(DPO(input$mid$Close), "DPO should throw error on partial NA")
}
