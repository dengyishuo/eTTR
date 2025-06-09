# RUnit tests for Ultimate Oscillator
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

# Ultimate Oscillator test suite
test.ultimateOscillator <- function() {
  suite <- defineTestSuite("ultimateOscillator",
    testfunctions = list(
      test.ultimateOscillator,
      test.ultimateOscillator.monthly.xts
    ),
    dir = "tests"
  )
  return(suite)
}

test.ultimateOscillator <- function() {
  # Handle data frame to matrix conversion for attributes
  ia <- input$all[, c("High", "Low", "Close")]
  it <- input$top[, c("High", "Low", "Close")]
  rn <- rownames(ia)
  rownames(ia) <- rownames(it) <- NULL
  oa <- ultimateOscillator(ia)
  names(oa) <- rn
  ot <- ultimateOscillator(it)
  names(ot) <- rn
  checkEqualsNumeric(oa, output$allUltOsc, "Ultimate Oscillator calculation error")
  checkEquals(attributes(oa), attributes(output$allUltOsc), "Ultimate Oscillator attributes mismatch")
  checkEqualsNumeric(ot, output$topUltOsc, "Ultimate Oscillator with NA input error")
  checkEquals(attributes(ot), attributes(output$topUltOsc), "Ultimate Oscillator attributes with NA mismatch")
  checkException(ultimateOscillator(input$mid$Close), "Ultimate Oscillator should throw error on invalid input")
}

test.ultimateOscillator.monthly.xts <- function() {
  stopifnot(requireNamespace("xts"))
  # Ultimate Oscillator on non-xts monthly data
  iam <- xts::to.monthly(input$all, name = NULL)[, c("High", "Low", "Close")]
  rn <- rownames(iam)
  rownames(iam) <- NULL
  oam <- ultimateOscillator(iam, c(2, 5, 8))
  # Ultimate Oscillator on xts monthly data
  xia <- xts::as.xts(input$all)
  xiam <- xts::to.monthly(xia, name = NULL)[, c("High", "Low", "Close")]
  xoam <- ultimateOscillator(xiam, c(2, 5, 8))
  checkEqualsNumeric(oam, xoam, "Ultimate Oscillator monthly xts error")
}
