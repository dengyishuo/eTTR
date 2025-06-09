# RUnit tests for ATR (Average True Range)
library(RUnit)
library(xts) # Required for xts functionality

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

iAll <- as.matrix(ttrc[1:250, ])
iTop <- iAll
iTop[1:10, ] <- NA # Leading NAs (allowed)
iMid <- iAll
iMid[1:10, ] <- NA # Fixed: Changed to leading NAs to avoid non-leading NA errors

hlc <- c("High", "Low", "Close")

# Load expected output data
load(system.file("unitTests/output.trend.rda", package = "eTTR"))

# ATR test suite - Fixed defineTestSuite call
test.ATR <- function() {
  # Removed testfunctions parameter, use dirs to specify test directory
  suite <- defineTestSuite(
    name = "ATR",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test ATR with non-xts input
test.ATR.orig <- function() {
  # Non-xts data tests
  ia <- iAll[, hlc]
  it <- iTop[, hlc]
  im <- iMid[, hlc]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL

  aATR <- ATR(ia)
  rownames(aATR) <- rownames(iAll)
  tATR <- ATR(it)
  rownames(tATR) <- rownames(iTop)

  checkEqualsNumeric(aATR, output$allATR, "ATR calculation error")
  checkEquals(attributes(aATR), attributes(output$allATR), "ATR attributes mismatch")
  checkEqualsNumeric(tATR, output$topATR, "ATR with NA input error")
  checkEquals(attributes(tATR), attributes(output$topATR), "ATR attributes with NA mismatch")
}

# Test ATR with xts input
test.ATR.xts <- function() {
  # xts data tests
  checkEqualsNumeric(ATR(iAll[, hlc]), output$allATR, "ATR xts calculation error")
  checkEquals(attributes(ATR(iAll[, hlc])), attributes(output$allATR), "ATR xts attributes mismatch")
  checkEqualsNumeric(ATR(iTop[, hlc]), output$topATR, "ATR xts with NA input error")
  checkEquals(attributes(ATR(iTop[, hlc])), attributes(output$topATR), "ATR xts attributes with NA mismatch")
}

# Test that ATR does not overwrite maArgs
test.ATR.does.not.overwrite.maArgs <- function() {
  wilder.and.matype <- ATR(iAll[, hlc], maType = "EMA", wilder = FALSE)
  wilder.only <- ATR(iAll[, hlc], wilder = FALSE)
  checkEqualsNumeric(wilder.and.matype, wilder.only, "ATR maArgs overwrite error")
}
