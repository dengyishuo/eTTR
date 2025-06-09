# RUnit tests for Chaikin Accumulation/Distribution
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

# Chaikin AD test suite
test.chaikinAD <- function() {
  suite <- defineTestSuite("chaikinAD",
    list(
      test.chaikinAD
    ),
    dir = "tests"
  )
  return(suite)
}

test.chaikinAD <- function() {
  checkEqualsNumeric(
    chaikinAD(
      iAll[, hlc],
      iAll[["Volume"]]
    ),
    output$allChaikinAD,
    "Chaikin AD calculation error"
  )
}
