# RUnit tests for Chaikin Money Flow (CMF)
library(RUnit)

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

iAll <- ttrc[1:250, ]
iTop <- iAll
iTop[1:10, ] <- NA
iMid <- iAll
iMid[9:20, ] <- NA

hlc <- c("High", "Low", "Close")

# Load output data
load(system.file("unitTests/output.volume.rda", package = "eTTR"))

# CMF test suite
test.CMF <- function() {
  suite <- defineTestSuite("CMF",
    testfunctions = list(
      test.CMF
    ),
    dir = "tests"
  )
  return(suite)
}

test.CMF <- function() {
  ia <- iAll[, hlc]
  it <- iTop[, hlc]
  checkEqualsNumeric(CMF(ia, iAll[["Volume"]]), output$allCMF, "CMF calculation error")
  checkEqualsNumeric(CMF(it, iTop[["Volume"]]), output$topCMF, "CMF with NA input error")
  checkException(CMF(iMid[, hlc], iMid[["Volume"]]), "CMF should throw error on partial NA")
  checkException(CMF(iAll[, hlc], iMid[["Volume"]]), "CMF should throw error on mismatched volume")
  checkException(CMF(iMid[, hlc], iAll[["Volume"]]), "CMF should throw error on mismatched price")
}
