# RUnit tests for Money Flow Index (MFI)
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

# MFI test suite
test.MFI <- function() {
  suite <- defineTestSuite("MFI",
    testfunctions = list(
      test.MFI,
      test.MFI.when.volume.does.not.change
    ),
    dir = "tests"
  )
  return(suite)
}

test.MFI <- function() {
  ia <- iAll[, hlc]
  it <- iTop[, hlc]
  checkEqualsNumeric(MFI(ia, iAll[["Volume"]]), output$allMFI, "MFI calculation error")
  checkEqualsNumeric(MFI(it, iTop[["Volume"]]), output$topMFI, "MFI with NA input error")
  checkException(MFI(iMid[, hlc], iMid[["Volume"]]), "MFI should throw error on partial NA")
  checkException(MFI(iAll[, hlc], iMid[["Volume"]]), "MFI should throw error on mismatched volume")
  checkException(MFI(iMid[, hlc], iAll[["Volume"]]), "MFI should throw error on mismatched price")
}

test.MFI.when.volume.does.not.change <- function() {
  x <- structure(c(
    6284.19, 6284.19, 6284.19, 6284.19, 6284.19, 6285.22,
    6285.96, 6287.54, 6287.84, 6287.89, 6288.95, 6284.19, 6284.19, 6284.19,
    6284.19, 6284.19, 6283.98, 6284.20, 6285.54, 6286.71, 6286.58, 6286.75,
    6284.19, 6284.19, 6284.19, 6284.19, 6284.19, 6284.46, 6285.54, 6287.47,
    6286.92, 6286.82, 6288.95, 9171293400, 9171293400, 9171293400, 9171293400,
    9171293400, 1650189487, 1796244384, 1864666606, 1845475611, 1831082797,
    1918533018
  ), .Dim = c(11L, 4L))

  o <- c(NA, NA, NA, 50, 50, 100, 100, 100, 100, 66.95494, 67.27551)

  m <- MFI(x[, -4], x[, 4], n = 3)
  checkEqualsNumeric(m, o, "MFI with constant volume error")
}
