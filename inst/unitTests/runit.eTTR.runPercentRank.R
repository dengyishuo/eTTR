# RUnit tests for runPercentRank
library(RUnit)

# Create input data for runPercentRank
xdata <- c(
  7.9, 5.2, 17.5, -12.7, 22, 4.3, -15.7, -9.3, 0.6, 0,
  -22.8, 7.6, -5.5, 1.7, 5.6, 15.1, 6.6, 11.2, -7.8, -4.3
)
xrank10_1 <- c(
  NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.4, 0.1, 0.8,
  0.5, 0.7, 0.9, 1, 0.8, 0.9, 0.2, 0.4
)

xrank10_0 <- c(
  NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.3, 0, 0.7,
  0.4, 0.6, 0.8, 0.9, 0.7, 0.8, 0.1, 0.3
)

# runPercentRank test suite
test.runPercentRank <- function() {
  suite <- defineTestSuite("runPercentRank",
    testfunctions = list(
      test.runPercentRank_exact.multiplier_bounds,
      test.runPercentRank_exact.multiplier_eq0,
      test.runPercentRank_exact.multiplier_eq0.5,
      test.runPercentRank_exact.multiplier_eq1,
      test.runPercentRank_cumulTRUE_exact.multiplier_eq0,
      test.runPercentRank_cumulTRUE_exact.multiplier_eq0.5,
      test.runPercentRank_cumulTRUE_exact.multiplier_eq1
    ),
    dir = "tests"
  )
  return(suite)
}

test.runPercentRank_exact.multiplier_bounds <- function() {
  x <- input$all$Close
  checkException(runPercentRank(x, 10, exact.multiplier = -0.1), "runPercentRank should reject invalid multiplier")
  checkException(runPercentRank(x, 10, exact.multiplier = 1.1), "runPercentRank should reject invalid multiplier")
}

test.runPercentRank_exact.multiplier_eq0 <- function() {
  xrank <- round(xrank10_0, 2)
  checkIdentical(xrank, runPercentRank(xdata, 10, FALSE, 0), "runPercentRank exact.multiplier=0 error")
}

test.runPercentRank_exact.multiplier_eq0.5 <- function() {
  xrank <- round(xrank10_0 + 0.05, 2)
  checkIdentical(xrank, runPercentRank(xdata, 10, FALSE, 0.5), "runPercentRank exact.multiplier=0.5 error")
}

test.runPercentRank_exact.multiplier_eq1 <- function() {
  xrank <- round(xrank10_0 + 0.1, 2)
  checkIdentical(xrank, runPercentRank(xdata, 10, FALSE, 1), "runPercentRank exact.multiplier=1 error")
}

test.runPercentRank_cumulTRUE_exact.multiplier_eq0 <- function() {
  xrank <- c(
    0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
    4, 7, 10, 13, 11, 14, 4, 6
  ) / 1:20
  xrank[1:9] <- NA
  xrank[10] <- 0
  checkIdentical(xrank, runPercentRank(xdata, 10, TRUE, 0), "runPercentRank cumulative exact.multiplier=0 error")
}

test.runPercentRank_cumulTRUE_exact.multiplier_eq0.5 <- function() {
  xrank <- (c(
    0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
    4, 7, 10, 13, 11, 14, 4, 6
  ) + 0.5) / 1:20
  xrank[1:9] <- NA
  xrank[10] <- 0.5
  checkIdentical(xrank, runPercentRank(xdata, 10, TRUE, 0.5), "runPercentRank cumulative exact.multiplier=0.5 error")
}

test.runPercentRank_cumulTRUE_exact.multiplier_eq1 <- function() {
  xrank <- (c(
    0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
    4, 7, 10, 13, 11, 14, 4, 6
  ) + 1) / 1:20
  xrank[1:9] <- NA
  xrank[10] <- 1
  checkIdentical(xrank, runPercentRank(xdata, 10, TRUE, 1), "runPercentRank cumulative exact.multiplier=1 error")
}
# RUnit tests for runPercentRank
library(RUnit)

# Create input data for runPercentRank
xdata <- c(
  7.9, 5.2, 17.5, -12.7, 22, 4.3, -15.7, -9.3, 0.6, 0,
  -22.8, 7.6, -5.5, 1.7, 5.6, 15.1, 6.6, 11.2, -7.8, -4.3
)
xrank10_1 <- c(
  NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.4, 0.1, 0.8,
  0.5, 0.7, 0.9, 1, 0.8, 0.9, 0.2, 0.4
)

xrank10_0 <- c(
  NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.3, 0, 0.7,
  0.4, 0.6, 0.8, 0.9, 0.7, 0.8, 0.1, 0.3
)

# runPercentRank test suite - Fixed defineTestSuite call
test.runPercentRank <- function() {
  # Removed testfunctions parameter, use dirs to specify test directory
  suite <- defineTestSuite(
    name = "runPercentRank",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test runPercentRank with exact.multiplier bounds checking
test.runPercentRank_exact.multiplier_bounds <- function() {
  # Removed dependency on input$all$Close, use xdata instead
  checkException(
    runPercentRank(xdata, 10, exact.multiplier = -0.1),
    "runPercentRank should reject invalid multiplier < 0"
  )
  checkException(
    runPercentRank(xdata, 10, exact.multiplier = 1.1),
    "runPercentRank should reject invalid multiplier > 1"
  )
}

# Test runPercentRank with exact.multiplier = 0
test.runPercentRank_exact.multiplier_eq0 <- function() {
  xrank <- round(xrank10_0, 2)
  checkIdentical(
    xrank, runPercentRank(xdata, 10, FALSE, 0),
    "runPercentRank exact.multiplier=0 calculation error"
  )
}

# Test runPercentRank with exact.multiplier = 0.5
test.runPercentRank_exact.multiplier_eq0.5 <- function() {
  xrank <- round(xrank10_0 + 0.05, 2)
  checkIdentical(
    xrank, runPercentRank(xdata, 10, FALSE, 0.5),
    "runPercentRank exact.multiplier=0.5 calculation error"
  )
}

# Test runPercentRank with exact.multiplier = 1
test.runPercentRank_exact.multiplier_eq1 <- function() {
  xrank <- round(xrank10_0 + 0.1, 2)
  checkIdentical(
    xrank, runPercentRank(xdata, 10, FALSE, 1),
    "runPercentRank exact.multiplier=1 calculation error"
  )
}

# Test runPercentRank with cumulative=TRUE and exact.multiplier=0
test.runPercentRank_cumulTRUE_exact.multiplier_eq0 <- function() {
  xrank <- c(
    0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
    4, 7, 10, 13, 11, 14, 4, 6
  ) / 1:20
  xrank[1:9] <- NA
  xrank[10] <- 0
  checkIdentical(
    xrank, runPercentRank(xdata, 10, TRUE, 0),
    "runPercentRank cumulative exact.multiplier=0 calculation error"
  )
}

# Test runPercentRank with cumulative=TRUE and exact.multiplier=0.5
test.runPercentRank_cumulTRUE_exact.multiplier_eq0.5 <- function() {
  xrank <- (c(
    0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
    4, 7, 10, 13, 11, 14, 4, 6
  ) + 0.5) / 1:20
  xrank[1:9] <- NA
  xrank[10] <- 0.5
  checkIdentical(
    xrank, runPercentRank(xdata, 10, TRUE, 0.5),
    "runPercentRank cumulative exact.multiplier=0.5 calculation error"
  )
}

# Test runPercentRank with cumulative=TRUE and exact.multiplier=1
test.runPercentRank_cumulTRUE_exact.multiplier_eq1 <- function() {
  xrank <- (c(
    0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
    4, 7, 10, 13, 11, 14, 4, 6
  ) + 1) / 1:20
  xrank[1:9] <- NA
  xrank[10] <- 1
  checkIdentical(
    xrank, runPercentRank(xdata, 10, TRUE, 1),
    "runPercentRank cumulative exact.multiplier=1 calculation error"
  )
}
