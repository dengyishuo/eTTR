# RUnit tests for runSum and wilderSum with non-leading NA error testing
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

# runSum and wilderSum test suite - Fixed defineTestSuite call
test.runSum.wilderSum <- function() {
  suite <- defineTestSuite(
    name = "runSum_wilderSum",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test runSum functionality
test.runSum <- function() {
  x <- input$all$Close
  x_top <- input$top$Close

  # Validate NA checks
  checkTrue(safe_na_check(x), "runSum NA check failed")
  checkTrue(safe_na_check(x_top), "runSum with NA input NA check failed")

  # Base runSum tests
  checkEqualsNumeric(runSum(x), output$allSum, "runSum calculation error")
  checkEquals(attributes(runSum(x)), attributes(output$allSum), "runSum attributes mismatch")

  # runSum with leading NA tests
  checkEqualsNumeric(runSum(x_top), output$topSum, "runSum with NA input error")
  checkEquals(attributes(runSum(x_top)), attributes(output$topSum), "runSum attributes with NA mismatch")

  # Exception tests with non-leading NA
  checkException(runSum(input$mid$Close), "runSum should throw error on non-leading NA")
  checkException(runSum(input$all[, 1:2]), "runSum should reject multi-column input")

  # Cumulative runSum test
  checkEqualsNumeric(
    tail(runSum(x, 250), 1),
    sum(x),
    "runSum cumulative calculation error"
  )

  # N parameter tests
  checkException(runSum(x, n = -1), "runSum should reject negative n")
  checkException(runSum(x, n = NROW(input$all) + 1), "runSum should reject n > length")
}

# Test wilderSum functionality
test.wilderSum <- function() {
  x <- input$all$Close
  x_top <- input$top$Close

  # Validate NA checks
  checkTrue(safe_na_check(x), "wilderSum NA check failed")
  checkTrue(safe_na_check(x_top), "wilderSum with NA input NA check failed")

  # Base wilderSum tests
  checkEqualsNumeric(wilderSum(x), output$allwSum, "wilderSum calculation error")
  checkEquals(attributes(wilderSum(x)), attributes(output$allwSum), "wilderSum attributes mismatch")

  # wilderSum with leading NA tests
  checkEqualsNumeric(wilderSum(x_top), output$topwSum, "wilderSum with NA input error")
  checkEquals(attributes(wilderSum(x_top)), attributes(output$topwSum), "wilderSum attributes with NA mismatch")

  # Exception tests with non-leading NA
  checkException(wilderSum(input$mid$Close), "wilderSum should throw error on non-leading NA")
  checkException(wilderSum(input$all[, 1:2]), "wilderSum should reject multi-column input")

  # N parameter tests
  checkException(wilderSum(x, n = -1), "wilderSum should reject negative n")
  checkException(wilderSum(x, n = NROW(input$all) + 1), "wilderSum should reject n > length")
}
