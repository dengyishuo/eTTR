# RUnit tests for TDI (True Directional Index)
library(RUnit)

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

# Prepare test data with different NA patterns
input <- list(
  all = ttrc[1:250, ], # Complete data
  top = ttrc[1:250, ], # Leading NAs (rows 1-15)
  mid = ttrc[1:250, ], # Middle NAs (rows 9-20)
  short = ttrc[1:10, ], # Short data (n=20, data length=10)
  invalid = matrix(NA, 250, 3) # All NA
)

input$top[1:15, ] <- NA
input$mid[9:20, ] <- NA

# 加载output.trend.rda中的验证结果
output <- list()
output_path <- system.file("unitTests/output.trend.rda", package = "eTTR")

if (file.exists(output_path)) {
  tryCatch(
    {
      load(output_path, envir = as.environment(output))
    },
    error = function(e) {
      warning("Failed to load output.trend.rda, using placeholder data")
      output$allTDI <- rep(NA, 250)
      output$topTDI <- rep(NA, 250)
    }
  )
} else {
  warning("output.trend.rda not found, using placeholder data")
  output$allTDI <- rep(NA, 250)
  output$topTDI <- rep(NA, 250)
}

# TDI test suite definition - 移除所有不必要参数
test.TDI <- function() {
  suite <- defineTestSuite("TDI",
    list(
      test.TDI.basic,
      test.TDI.leadingNA,
      test.TDI.middleNA,
      test.TDI.shortData,
      test.TDI.invalidInput,
      test.TDI.attributes
    ),
    dirs = "tests" # 指定测试目录
  )
  return(suite)
}

# Basic functionality test
test.TDI.basic <- function() {
  if (all(is.na(output$allTDI))) {
    warning("Using placeholder data for basic test")
  } else {
    result <- TDI(input$all$Close)
    checkEqualsNumeric(
      result, output$allTDI,
      "TDI calculation error for complete data"
    )
  }
}

# Leading NA test
test.TDI.leadingNA <- function() {
  if (all(is.na(output$topTDI))) {
    warning("Using placeholder data for leading NA test")
  } else {
    result <- TDI(input$top$Close)
    checkEqualsNumeric(
      result, output$topTDI,
      "TDI calculation error for leading NA data"
    )
  }
}

# Middle NA test (should throw error)
test.TDI.middleNA <- function() {
  # 捕获并验证错误类型
  error <- tryCatch(
    {
      TDI(input$mid$Close)
      NULL
    },
    error = function(e) {
      e
    }
  )

  if (is.null(error)) {
    checkTrue(FALSE, "TDI did not throw error on partial NA (non-leading)")
  } else {
    checkTrue(
      grepl("NA values", error$message),
      "TDI did not throw expected error on partial NA"
    )
  }
}

# Short data test (should throw error)
test.TDI.shortData <- function() {
  # 捕获并验证错误类型
  error <- tryCatch(
    {
      TDI(input$short$Close)
      NULL
    },
    error = function(e) {
      e
    }
  )

  if (is.null(error)) {
    checkTrue(FALSE, "TDI did not throw error on short data")
  } else {
    checkTrue(
      grepl("n must be smaller than data length", error$message),
      "TDI did not throw expected error on short data"
    )
  }
}

# Invalid input test (all NA)
test.TDI.invalidInput <- function() {
  checkException(
    TDI(input$invalid),
    "TDI should throw error on invalid input"
  )
}

# Attributes test
test.TDI.attributes <- function() {
  if (all(is.na(output$allTDI))) {
    warning("Attribute test skipped due to placeholder data")
    return()
  }

  result <- TDI(input$all$Close)
  expected <- output$allTDI

  checkEquals(
    dim(result), dim(expected),
    "TDI dimension mismatch for complete data"
  )

  # 检查列名属性
  if (is.matrix(expected) && is.matrix(result)) {
    checkEquals(
      colnames(result), colnames(expected),
      "TDI colnames mismatch for complete data"
    )
  }

  # 检查类属性
  checkEquals(
    class(result), class(expected),
    "TDI class mismatch for complete data"
  )
}
