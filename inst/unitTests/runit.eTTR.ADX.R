# RUnit tests for ADX (Average Directional Index)
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

# ADX test suite
test.ADX <- function() {
  suite <- defineTestSuite("ADX",
    list(
      test.ADX,
      test.ADX.does.not.overwrite.maArgs
    ),
    dir = "tests"
  )
  suite
}

test.ADX <- function() {
  checkEqualsNumeric(ADX(iAll[, hlc]), output$allADX, "ADX calculation error")
  checkEquals(attributes(ADX(iAll[, hlc])), attributes(output$allADX), "ADX attributes mismatch")
  checkEqualsNumeric(ADX(iTop[, hlc]), output$topADX, "ADX with NA input error")
  checkEquals(attributes(ADX(iTop[, hlc])), attributes(output$topADX), "ADX attributes with NA mismatch")
}

test.ADX.does.not.overwrite.maArgs <- function() {
  wilder.and.matype <- ADX(iAll[, hlc], maType = "EMA", wilder = FALSE)
  wilder.only <- ADX(iAll[, hlc], wilder = FALSE)
  checkEqualsNumeric(wilder.and.matype, wilder.only, "ADX maArgs overwrite error")
}
