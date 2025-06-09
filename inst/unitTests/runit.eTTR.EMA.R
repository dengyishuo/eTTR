# RUnit tests for Exponential Moving Average (EMA)
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

# EMA test suite
test.EMA <- function() {
  suite <- defineTestSuite("EMA",
    testfunctions = list(
      test.EMA,
      test.EMA.n.ratio,
      test.EMA.ratio.eq.0,
      test.EMA.wilder
    ),
    dir = "tests"
  )
  return(suite)
}

test.EMA <- function() {
  checkEqualsNumeric(EMA(input$all$Close), output$allEMA, "EMA calculation error")
  checkEquals(attributes(EMA(input$all$Close)), attributes(output$allEMA), "EMA attributes mismatch")
  checkEqualsNumeric(EMA(input$top$Close), output$topEMA, "EMA with NA input error")
  checkEquals(attributes(EMA(input$top$Close)), attributes(output$topEMA), "EMA attributes with NA mismatch")
  checkException(EMA(input$mid$Close), "EMA should throw error on partial NA")
  checkException(EMA(input$all[, 1:2]), "EMA should reject multi-column input")
  checkException(EMA(input$all$Close, n = -1), "EMA should reject negative n")
  checkException(EMA(input$all$Close, n = NROW(input$all) + 1), "EMA should reject n > length")
}

test.EMA.n.ratio <- function() {
  out <- 0:9 * 1.0
  is.na(out) <- 1:2
  checkEqualsNumeric(EMA(1:10, ratio = 0.5), out, "EMA n/ratio equivalence error")
  checkEqualsNumeric(EMA(1:10, n = 3), out, "EMA n/ratio equivalence error")
  checkEqualsNumeric(EMA(1:10, n = 3, ratio = 0.5), out, "EMA n/ratio equivalence error")
}

test.EMA.ratio.eq.0 <- function() {
  checkException(EMA(1:10, ratio = 0.0), "EMA should throw error on ratio=0")
}

test.EMA.wilder <- function() {
  checkEqualsNumeric(EMA(input$all$Close, wilder = TRUE), output$allEMAwilder, "Wilder EMA error")
  checkEquals(attributes(EMA(input$all$Close, wilder = TRUE)), attributes(output$allEMAwilder), "Wilder EMA attributes mismatch")
  checkEqualsNumeric(EMA(input$top$Close, wilder = TRUE), output$topEMAwilder, "Wilder EMA with NA error")
  checkException(EMA(input$mid$Close, wilder = TRUE), "Wilder EMA should throw error on partial NA")
}
