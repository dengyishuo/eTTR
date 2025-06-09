# RUnit tests for On Balance Volume (OBV)
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

cl <- "Close"

# Load output data
load(system.file("unitTests/output.volume.rda", package = "eTTR"))

# OBV test suite
test.OBV <- function() {
  suite <- defineTestSuite("OBV",
    testfunctions = list(
      test.OBV
    ),
    dir = "tests"
  )
  return(suite)
}

test.OBV <- function() {
  checkEqualsNumeric(OBV(iAll[[cl]], iAll[["Volume"]]), output$allOBV, "OBV calculation error")
}
