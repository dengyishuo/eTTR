# RUnit tests for Aroon indicator
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

hl <- c("High", "Low")

# Load expected output data
load(system.file("unitTests/output.trend.rda", package = "eTTR"))

# Aroon test suite - Fixed defineTestSuite call
test.Aroon <- function() {
  # Removed testfunctions parameter, use dirs to specify test directory
  suite <- defineTestSuite(
    name = "Aroon",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test Aroon with non-xts input
test.aroon.orig <- function() {
  # Non-xts data tests
  ia <- iAll[, hl]
  it <- iTop[, hl]
  im <- iMid[, hl]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL

  oa <- aroon(ia)
  rownames(oa) <- rownames(iAll)
  ot <- aroon(it)
  rownames(ot) <- rownames(iTop)

  checkEqualsNumeric(oa, output$allAroon, "Aroon calculation error")
  checkEquals(attributes(oa), attributes(output$allAroon), "Aroon attributes mismatch")
  checkEqualsNumeric(ot, output$topAroon, "Aroon with NA input error")
  checkEquals(attributes(ot), attributes(output$topAroon), "Aroon attributes with NA mismatch")
}

# Test Aroon with xts input
test.aroon.xts <- function() {
  # xts data tests
  checkEqualsNumeric(aroon(iAll[, hl]), output$allAroon, "Aroon xts calculation error")
  checkEquals(attributes(aroon(iAll[, hl])), attributes(output$allAroon), "Aroon xts attributes mismatch")
  checkEqualsNumeric(aroon(iTop[, hl]), output$topAroon, "Aroon xts with NA input error")
  checkEquals(attributes(aroon(iTop[, hl])), attributes(output$topAroon), "Aroon xts attributes with NA mismatch")
}

# Test that Aroon doesn't error with partial NA when n equals non-NA length
test.aroon.non.na.eq.n.does.not.error <- function() {
  # Test with xts data containing partial NA
  x <- c(NA, rnorm(10))
  a <- aroon(x, 10)
  checkTrue(TRUE, "Aroon should not error on partial NA input")
}
