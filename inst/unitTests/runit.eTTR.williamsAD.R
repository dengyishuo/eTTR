# RUnit tests for Williams' Accumulation/Distribution
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

# Williams AD test suite
test.williamsAD <- function() {
  suite <- defineTestSuite("williamsAD",
    testfunctions = list(
      test.williamsAD
    ),
    dir = "tests"
  )
  return(suite)
}

test.williamsAD <- function() {
  ia <- iAll[, hlc]
  checkEqualsNumeric(williamsAD(ia), output$allWilliamsAD, "Williams AD calculation error")
}
