# RUnit tests for runVar, runSD, and runMAD with non-leading NA error testing
library(RUnit)

# Helper function for NA checking
safe_na_check <- function(x) {
  nas <- sum(is.na(x))
  all(!is.na(x[-(1:nas)]))
}

# Create input data with non-leading NAs for error testing
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list(all = ttrc[1:250, ], top = ttrc[1:250, ], mid = ttrc[1:250, ])
input$top[1:10, ] <- NA # Leading NAs (allowed)
input$mid[9:20, ] <- NA # Non-leading NAs (triggers error)

# Load output data
load(system.file("unitTests/output.runFun.rda", package = "eTTR"))

# runVar, runSD, and runMAD test suite - Fixed defineTestSuite call
test.runVar.sd.mad <- function() {
  suite <- defineTestSuite(
    name = "runVar_sd_mad",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test runVar functionality
test.runVar <- function() {
  x <- input$all$Close
  x_top <- input$top$Close

  # Validate NA checks
  checkTrue(safe_na_check(x), "runVar NA check failed")
  checkTrue(safe_na_check(x_top), "runVar with NA input NA check failed")

  # Base runVar tests
  checkEqualsNumeric(runVar(x), output$allVar, "runVar calculation error")
  checkEquals(attributes(runVar(x)), attributes(output$allVar), "runVar attributes mismatch")

  # runVar with leading NA tests
  checkEqualsNumeric(runVar(x_top), output$topVar, "runVar with NA input error")
  checkEquals(attributes(runVar(x_top)), attributes(output$topVar), "runVar attributes with NA mismatch")

  # Exception tests with non-leading NA
  checkException(runVar(input$mid$Close), "runVar should throw error on non-leading NA")
  checkException(runVar(input$all[, 1:2], input$all$Low), "runVar should reject invalid input")

  # N parameter tests
  checkException(runVar(x, n = -1), "runVar should reject negative n")
  checkException(runVar(x, n = NROW(input$all) + 1), "runVar should reject n > length")

  # Cumulative runVar test
  checkEqualsNumeric(
    tail(runVar(x, n = 250), 1),
    var(x),
    "runVar cumulative calculation error"
  )
}

# Test runSD functionality
test.runSD <- function() {
  x <- input$all$Close
  x_top <- input$top$Close

  # Validate NA checks
  checkTrue(safe_na_check(x), "runSD NA check failed")
  checkTrue(safe_na_check(x_top), "runSD with NA input NA check failed")

  # Base runSD tests
  checkEqualsNumeric(runSD(x), output$allSD, "runSD calculation error")
  checkEquals(attributes(runSD(x)), attributes(output$allSD), "runSD attributes mismatch")

  # runSD with leading NA tests
  checkEqualsNumeric(runSD(x_top), output$topSD, "runSD with NA input error")
  checkEquals(attributes(runSD(x_top)), attributes(output$topSD), "runSD attributes with NA mismatch")

  # Exception tests with non-leading NA
  checkException(runSD(input$mid$Close), "runSD should throw error on non-leading NA")
  checkException(runSD(input$all[, 1:2]), "runSD should reject multi-column input")

  # N parameter tests
  checkException(runSD(x, n = -1), "runSD should reject negative n")
  checkException(runSD(x, n = NROW(input$all) + 1), "runSD should reject n > length")

  # Cumulative runSD test
  checkEqualsNumeric(
    tail(runSD(x, 250), 1),
    sd(x),
    "runSD cumulative calculation error"
  )
}

# Test runMAD functionality
test.runMAD <- function() {
  x <- input$all$Close
  x_top <- input$top$Close

  # Validate NA checks
  checkTrue(safe_na_check(x), "runMAD NA check failed")
  checkTrue(safe_na_check(x_top), "runMAD with NA input NA check failed")

  # Base runMAD tests
  checkEqualsNumeric(runMAD(x), output$allMAD, "runMAD calculation error")
  checkEquals(attributes(runMAD(x)), attributes(output$allMAD), "runMAD attributes mismatch")

  # runMAD with leading NA tests
  checkEqualsNumeric(runMAD(x_top), output$topMAD, "runMAD with NA input error")
  checkEquals(attributes(runMAD(x_top)), attributes(output$topMAD), "runMAD attributes with NA mismatch")

  # Exception tests with non-leading NA
  checkException(runMAD(input$mid$Close), "runMAD should throw error on non-leading NA")
  checkException(runMAD(input$all[, 1:2]), "runMAD should reject multi-column input")

  # N parameter tests
  checkException(runMAD(x, n = -1), "runMAD should reject negative n")
  checkException(runMAD(x, n = NROW(input$all) + 1), "runMAD should reject n > length")

  # Cumulative runMAD test
  checkEqualsNumeric(
    tail(runMAD(x, 250), 1),
    mad(x),
    "runMAD cumulative calculation error"
  )
}
