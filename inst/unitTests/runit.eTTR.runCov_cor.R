# RUnit tests for runCov and runCor with non-leading NA error testing
library(RUnit)

# Helper function to handle NA checks
safe_na_check <- function(x, y) {
  nas_x <- sum(is.na(x))
  nas_y <- sum(is.na(y))

  valid_x <- seq_along(x)[-(1:nas_x)]
  valid_y <- seq_along(y)[-(1:nas_y)]

  common_valid <- intersect(valid_x, valid_y)

  if (length(common_valid) == 0) {
    return(TRUE)
  }

  all(!is.na(x[common_valid]) & !is.na(y[common_valid]))
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

# runCov and runCor test suite - Fixed defineTestSuite call
test.runCov.cor <- function() {
  suite <- defineTestSuite(
    name = "runCov_cor",
    dirs = system.file("unitTests", package = "eTTR")
  )
  return(suite)
}

# Test runCov functionality
test.runCov <- function() {
  x <- input$all$High
  y <- input$all$Low
  x_top <- input$top$High
  y_top <- input$top$Low

  # Validate NA checks
  checkTrue(safe_na_check(x, y), "runCov NA check failed")
  checkTrue(safe_na_check(x_top, y_top), "runCov with NA input NA check failed")

  # Base runCov tests
  checkEqualsNumeric(runCov(x, y), output$allCov, "runCov calculation error")
  checkEquals(attributes(runCov(x, y)), attributes(output$allCov), "runCov attributes mismatch")

  # runCov with leading NA tests
  checkEqualsNumeric(runCov(x_top, y_top), output$topCov, "runCov with NA input error")
  checkEquals(attributes(runCov(x_top, y_top)), attributes(output$topCov), "runCov attributes with NA mismatch")

  # Exception tests with non-leading NA
  checkException(runCov(input$mid$High, input$mid$Low), "runCov should throw error on non-leading NA")
  checkException(runCov(x), "runCov should require two vectors")
  checkException(runCov(input$all[, 1:2], y), "runCov should reject multi-column input")
  checkException(runCov(x, n = -1), "runCov should reject negative n")
  checkException(runCov(x, n = NROW(input$all) + 1), "runCov should reject n > length")

  # Cumulative runCov test
  checkEqualsNumeric(
    tail(runCov(x, y, 250), 1),
    cov(x, y),
    "runCov cumulative calculation error"
  )
}

# Test runCor functionality
test.runCor <- function() {
  x <- input$all$High
  y <- input$all$Low
  x_top <- input$top$High
  y_top <- input$top$Low

  # Validate NA checks
  checkTrue(safe_na_check(x, y), "runCor NA check failed")
  checkTrue(safe_na_check(x_top, y_top), "runCor with NA input NA check failed")

  # Base runCor tests
  checkEqualsNumeric(runCor(x, y), output$allCor, "runCor calculation error")
  checkEquals(attributes(runCor(x, y)), attributes(output$allCor), "runCor attributes mismatch")

  # runCor with leading NA tests
  checkEqualsNumeric(runCor(x_top, y_top), output$topCor, "runCor with NA input error")
  checkEquals(attributes(runCor(x_top, y_top)), attributes(output$topCor), "runCor attributes with NA mismatch")

  # Exception tests with non-leading NA
  checkException(runCor(input$mid$High, input$mid$Low), "runCor should throw error on non-leading NA")
  checkException(runCor(x), "runCor should require two vectors")
  checkException(runCor(input$all[, 1:2], y), "runCor should reject multi-column input")
  checkException(runCor(x, n = -1), "runCor should reject negative n")
  checkException(runCor(x, n = NROW(input$all) + 1), "runCor should reject n > length")

  # Cumulative runCor test
  checkEqualsNumeric(
    tail(runCor(x, y, 250), 1),
    cor(x, y),
    "runCor cumulative calculation error"
  )
}
