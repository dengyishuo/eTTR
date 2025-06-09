# RUnit tests for runMean and runMedian with non-leading NA error testing
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
input$top[1:10, ] <- NA  # Leading NAs (allowed)
input$mid[9:20, ] <- NA  # Non-leading NAs (triggers error)

# Load output data
load(system.file("unitTests/output.runFun.rda", package = "eTTR"))

# runMean and runMedian test suite - Fixed defineTestSuite call
test.runMean.median <- function() {
  suite <- defineTestSuite(
    name = "runMean_median",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test runMean functionality
test.runMean <- function() {
  x <- input$all$Close
  x_top <- input$top$Close

  # Validate NA checks
  checkTrue(safe_na_check(x), "runMean NA check failed")
  checkTrue(safe_na_check(x_top), "runMean with NA input NA check failed")

  # Base runMean tests
  checkEqualsNumeric(runMean(x), output$allMean, "runMean calculation error")
  checkEquals(attributes(runMean(x)), attributes(output$allMean), "runMean attributes mismatch")

  # runMean with leading NA tests
  checkEqualsNumeric(runMean(x_top), output$topMean, "runMean with NA input error")
  checkEquals(attributes(runMean(x_top)), attributes(output$topMean), "runMean attributes with NA mismatch")

  # Exception tests with non-leading NA
  checkException(runMean(input$mid$Close), "runMean should throw error on non-leading NA")
  checkException(runMean(input$all[, 1:2]), "runMean should reject multi-column input")
  checkException(runMean(x, n = -1), "runMean should reject negative n")
  checkException(runMean(x, n = NROW(input$all) + 1), "runMean should reject n > length")

  # Cumulative runMean test
  checkEqualsNumeric(
    tail(runMean(x, 250), 1),
    mean(x),
    "runMean cumulative calculation error"
  )
}

# Test runMean with cumulative argument
test.runMean.cumulative <- function() {
  x <- input$all$Close
  ttr <- runMean(x, 5, TRUE)
  base <- cumsum(x) / seq_along(x)
  is.na(base) <- 1:4  # First 4 values are NA due to window size 5

  # Validate NA checks
  checkTrue(safe_na_check(x), "runMean cumulative NA check failed")

  checkEqualsNumeric(base, ttr, "runMean cumulative calculation error")
}

# Test runMedian functionality
test.runMedian <- function() {
  x <- input$all$Close
  x_top <- input$top$Close

  # Validate NA checks
  checkTrue(safe_na_check(x), "runMedian NA check failed")
  checkTrue(safe_na_check(x_top), "runMedian with NA input NA check failed")

  # Base runMedian tests
  checkEqualsNumeric(runMedian(x), output$allMedian, "runMedian calculation error")
  checkEquals(attributes(runMedian(x)), attributes(output$allMedian), "runMedian attributes mismatch")

  # runMedian with leading NA tests
  checkEqualsNumeric(runMedian(x_top), output$topMedian, "runMedian with NA input error")
  checkEquals(attributes(runMedian(x_top)), attributes(output$topMedian), "runMedian attributes with NA mismatch")

  # Exception tests with non-leading NA
  checkException(runMedian(input$mid$Close), "runMedian should throw error on non-leading NA")
  checkException(runMedian(input$all[, 1:2]), "runMedian should reject multi-column input")
  checkException(runMedian(x, n = -1), "runMedian should reject negative n")
  checkException(runMedian(x, n = NROW(input$all) + 1), "runMedian should reject n > length")

  # Cumulative runMedian test
  checkEqualsNumeric(
    tail(runMedian(x, 250), 1),
    median(x),
    "runMedian cumulative calculation error"
  )
}

# Test runMedian with cumulative argument
test.runMedian.cumulative <- function() {
  x <- input$all$Close

  # Custom cumulative median function for comparison
  cummedian <- function(x) {
    med <- x * NA_real_
    for (i in seq_along(x)) {
      med[i] <- median(x[1:i])
    }
    med
  }

  base <- cummedian(x)
  is.na(base) <- 1:4  # First 4 values are NA due to window size 5
  ttr <- runMedian(x, 5, "mean", TRUE)

  # Validate NA checks
  checkTrue(safe_na_check(x), "runMedian cumulative NA check failed")

  checkEqualsNumeric(base, ttr, "runMedian cumulative calculation error")
}
