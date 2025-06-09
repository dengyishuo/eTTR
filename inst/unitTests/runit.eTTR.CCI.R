# RUnit tests for CCI (Commodity Channel Index)
library(RUnit)

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

iAll <- as.matrix(ttrc[1:250, ])
iTop <- iAll
iTop[1:10, ] <- NA
iMid <- iAll
iMid[9:20, ] <- NA

hlc <- c("High", "Low", "Close")

# Load output data
load(system.file("unitTests/output.trend.rda", package = "eTTR"))

# CCI test suite
test.CCI <- function() {
  suite <- defineTestSuite("CCI",
    testfunctions = list(
      test.CCI
    ),
    dir = "tests"
  )
  return(suite)
}

test.CCI <- function() {
  checkEqualsNumeric(CCI(iAll[, hlc]), output$allCCI, "CCI calculation error")
  checkEquals(attributes(CCI(iAll[, hlc])), attributes(output$allCCI), "CCI
              attributes mismatch")
  checkEqualsNumeric(CCI(iTop[, hlc]), output$topCCI, "CCI with NA input error")
  checkEquals(attributes(CCI(iTop[, hlc])), attributes(output$topCCI), "CCI
              attributes with NA mismatch")
}
