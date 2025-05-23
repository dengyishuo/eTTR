#
# RUnit tests eTTR moving averages
#

# test reclass works and throws error
# test xtsAttributes, both CLASS and USER
# test all.equal(CLASS) and !all.equal(CLASS) cases

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

# input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
# input$top[1:10,] <- NA
# input$mid[9:20,] <- NA

# iAll <- as.matrix(ttrc[1:250,])
iAll <- ttrc[1:250, ]
iTop <- iAll
iTop[1:10, ] <- NA
iMid <- iAll
iMid[9:20, ] <- NA

hl <- c("High", "Low")
hlc <- c("High", "Low", "Close")
cl <- "Close"

# Load output data
load(system.file("unitTests/output.volume.rda", package = "eTTR"))

#################################################

# On Balance Volume
test.OBV <- function() {
  checkEqualsNumeric(OBV(iAll$Close, iAll$Volume), output$allOBV)
  # checkEqualsNumeric( OBV(iTop[,cl], iTop[,'Volume']), output$topOBV )
  # checkException( OBV(iMid[,cl], iMid[,'Volume']) )
  # checkException( OBV(iAll[,cl], iMid[,'Volume']) )
  # checkException( OBV(iMid[,cl], iAll[,'Volume']) )
}

# Chaikin Accumulation / Distribution
test.chaikinAD <- function() {
  checkEqualsNumeric(chaikinAD(iAll[, hlc], iAll[, "Volume"]), output$allChaikinAD)
  # checkEqualsNumeric( chaikinAD(iTop[,hlc], iTop[,'Volume']), output$topChaikinAD )
  # checkException( chaikinAD(iMid[,hlc], iMid[,'Volume']) )
  # checkException( chaikinAD(iAll[,hlc], iMid[,'Volume']) )
  # checkException( chaikinAD(iMid[,hlc], iAll[,'Volume']) )
}

# Chaikin Money Flow
test.CMF <- function() {
  ia <- iAll[, hlc]
  rownames(ia) <- NULL
  it <- iTop[, hlc]
  rownames(it) <- NULL
  checkEqualsNumeric(CMF(ia, iAll[, "Volume"]), output$allCMF)
  checkEqualsNumeric(CMF(it, iTop[, "Volume"]), output$topCMF)
  checkException(CMF(iMid[, hlc], iMid[, "Volume"]))
  checkException(CMF(iAll[, hlc], iMid[, "Volume"]))
  checkException(CMF(iMid[, hlc], iAll[, "Volume"]))
}

# Money Flow Index
test.MFI <- function() {
  ia <- iAll[, hlc]
  rownames(ia) <- NULL
  it <- iTop[, hlc]
  rownames(it) <- NULL
  checkEqualsNumeric(MFI(ia, iAll[, "Volume"]), output$allMFI)
  checkEqualsNumeric(MFI(it, iTop[, "Volume"]), output$topMFI)
  checkException(MFI(iMid[, hlc], iMid[, "Volume"]))
  checkException(MFI(iAll[, hlc], iMid[, "Volume"]))
  checkException(MFI(iMid[, hlc], iAll[, "Volume"]))
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
  checkEqualsNumeric(m, o)
}

# Williams' Accumulation / Distribution
test.williamsAD <- function() {
  # non-xts
  ia <- iAll[, hlc]
  it <- iTop[, hlc]
  im <- iMid[, hlc]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL
  checkEqualsNumeric(williamsAD(ia), output$allWilliamsAD)
  # checkEqualsNumeric( williamsAD(iTop[,hlc]), output$topWilliamsAD )
  # checkException( williamsAD(iMid[,hlc]) )
}
