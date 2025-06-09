# RUnit tests for runMin and runMax with non-leading NA error testing
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

# runMin and runMax test suite - Fixed defineTestSuite call
test.runMin.max <- function() {
  suite <- defineTestSuite(
    name = "runMin_max",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test runMin functionality
test.runMin <- function() {
  x <- input$all$Close
  x_top <- input$top$Close

  # Validate NA checks
  checkTrue(safe_na_check(x), "runMin NA check failed")
  checkTrue(safe_na_check(x_top), "runMin with NA input NA check failed")

  # Base runMin tests
  checkEqualsNumeric(runMin(x), output$allMin, "runMin calculation error")
  checkEquals(attributes(runMin(x)), attributes(output$allMin), "runMin attributes mismatch")

  # runMin with leading NA tests
  checkEqualsNumeric(runMin(x_top), output$topMin, "runMin with NA input error")
  checkEquals(attributes(runMin(x_top)), attributes(output$topMin), "runMin attributes with NA mismatch")

  # Exception tests with non-leading NA
  checkException(runMin(input$mid$Close), "runMin should throw error on non-leading NA")
  checkException(runMin(input$all[, 1:2]), "runMin should reject multi-column input")
  checkException(runMin(x, n = -1), "runMin should reject negative n")
  checkException(runMin(x, n = NROW(input$all) + 1), "runMin should reject n > length")

  # Cumulative runMin test
  checkEqualsNumeric(
    tail(runMin(x, 250), 1),
    min(x),
    "runMin cumulative calculation error"
  )
}

# Test runMin with cumulative argument
test.runMin.cumulative <- function() {
  x <- input$all$Close
  ttr <- runMin(x, 1, TRUE)
  base <- cummin(x)

  # Validate NA checks
  checkTrue(safe_na_check(x), "runMin cumulative NA check failed")

  checkEqualsNumeric(base, ttr, "runMin cumulative calculation error")
}

# Test runMax functionality
test.runMax <- function() {
  x <- input$all$Close
  x_top <- input$top$Close

  # Validate NA checks
  checkTrue(safe_na_check(x), "runMax NA check failed")
  checkTrue(safe_na_check(x_top), "runMax with NA input NA check failed")

  # Base runMax tests
  checkEqualsNumeric(runMax(x), output$allMax, "runMax calculation error")
  checkEquals(attributes(runMax(x)), attributes(output$allMax), "runMax attributes mismatch")

  # runMax with leading NA tests
  checkEqualsNumeric(runMax(x_top), output$topMax, "runMax with NA input error")
  checkEquals(attributes(runMax(x_top)), attributes(output$topMax), "runMax attributes with NA mismatch")

  # Exception tests with non-leading NA
  checkException(runMax(input$mid$Close), "runMax should throw error on non-leading NA")
  checkException(runMax(input$all[, 1:2]), "runMax should reject multi-column input")
  checkException(runMax(x, n = -1), "runMax should reject negative n")
  checkException(runMax(x, n = NROW(input$all) + 1), "runMax should reject n > length")

  # Cumulative runMax test
  checkEqualsNumeric(
    tail(runMax(x, 250), 1),
    max(x),
    "runMax cumulative calculation error"
  )
}

# Test runMax with cumulative argument
test.runMax.cumulative <- function() {
  x <- input$all$Close
  ttr <- runMax(x, 1, TRUE)
  base <- cummax(x)

  # Validate NA checks
  checkTrue(safe_na_check(x), "runMax cumulative NA check failed")

  checkEqualsNumeric(base, ttr, "runMax cumulative calculation error")
}
