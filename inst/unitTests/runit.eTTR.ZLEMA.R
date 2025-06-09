# RUnit tests for Zero-Lag EMA (ZLEMA)
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

# ZLEMA test suite
test.ZLEMA <- function() {
  suite <- defineTestSuite("ZLEMA",
    testfunctions = list(
      test.ZLEMA,
      test.ZLEMA.n.ratio,
      test.ZLEMA.ratio.eq.0,
      test.EMA.non.na.eq.n.does.not.error,
      test.ma.on.xts.objects.have.colnames
    ),
    dir = "tests"
  )
  return(suite)
}

test.ZLEMA <- function() {
  checkEqualsNumeric(ZLEMA(input$all$Close), output$allZLEMA, "ZLEMA calculation error")
  checkEquals(attributes(ZLEMA(input$all$Close)), attributes(output$allZLEMA), "ZLEMA attributes mismatch")
  checkEqualsNumeric(ZLEMA(input$top$Close), output$topZLEMA, "ZLEMA with NA input error")
  checkEquals(attributes(ZLEMA(input$top$Close)), attributes(output$topZLEMA), "ZLEMA attributes with NA mismatch")
  checkException(ZLEMA(input$mid$Close), "ZLEMA should throw error on partial NA")
  checkException(ZLEMA(input$all[, 1:2]), "ZLEMA should reject multi-column input")
}

test.ZLEMA.n.ratio <- function() {
  out <- c(rep(NA, 6), 4.0, 6.0, 7.75, 9.3125)
  checkEqualsNumeric(ZLEMA(1:10, ratio = 0.25), out, "ZLEMA n/ratio equivalence error")
  checkEqualsNumeric(ZLEMA(1:10, n = 7), out, "ZLEMA n/ratio equivalence error")
  checkEqualsNumeric(ZLEMA(1:10, n = 7, ratio = 0.25), out, "ZLEMA n/ratio equivalence error")
}

test.ZLEMA.ratio.eq.0 <- function() {
  checkException(ZLEMA(1:10, ratio = 0.0), "ZLEMA should throw error on ratio=0")
}

test.EMA.non.na.eq.n.does.not.error <- function() {
  x <- c(NA, rnorm(10))
  e <- EMA(x, 10)
  z <- ZLEMA(x, 10)
  # If no error, test passes
  checkTrue(TRUE, "EMA/ZLEMA should not error on partial NA")
}

test.ma.on.xts.objects.have.colnames <- function() {
  x <- xts::as.xts(ttrc)
  p <- x[, "Close"]
  v <- x[, "Volume"]
  checkEquals("SMA", colnames(SMA(p)), "SMA colname error")
  checkEquals("EMA", colnames(EMA(p)), "EMA colname error")
  checkEquals("DEMA", colnames(DEMA(p)), "DEMA colname error")
  checkEquals("WMA", colnames(WMA(p)), "WMA colname error")
  checkEquals("EVWMA", colnames(EVWMA(p, v)), "EVWMA colname error")
  checkEquals("ZLEMA", colnames(ZLEMA(p)), "ZLEMA colname error")
  checkEquals("VWAP", colnames(VWAP(p, v)), "VWAP colname error")
  checkEquals("HMA", colnames(HMA(p)), "HMA colname error")
  checkEquals("ALMA", colnames(ALMA(p)), "ALMA colname error")
}
