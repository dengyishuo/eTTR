# RUnit tests for Chande Momentum Oscillator (CMO)
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

# CMO test suite
test.CMO <- function() {
  suite <- defineTestSuite("CMO",
    testfunctions = list(
      test.CMO
    ),
    dir = "tests"
  )
  return(suite)
}

test.CMO <- function() {
  checkEqualsNumeric(CMO(input$all$Close), output$allCMO, "CMO calculation error")
  checkEquals(attributes(CMO(input$all$Close)), attributes(output$allCMO), "CMO attributes mismatch")
  checkEqualsNumeric(CMO(input$top$Close), output$topCMO, "CMO with NA input error")
  checkEquals(attributes(CMO(input$top$Close)), attributes(output$topCMO), "CMO attributes with NA mismatch")
  checkException(CMO(input$mid$Close), "CMO should throw error on partial NA")
}
