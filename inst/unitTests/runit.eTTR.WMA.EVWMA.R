# Load required libraries
library(RUnit) # For unit testing
library(xts) # For time series operations

# Fixed safe_na_check function that ensures single logical value return
safe_na_check <- function(x, wts = NULL) {
  # Convert inputs to vectors
  x <- as.vector(x)
  if (!is.null(wts)) {
    wts <- as.vector(wts)
  }

  # Case without weights
  if (is.null(wts)) {
    nas <- sum(is.na(x))
    if (nas >= length(x)) {
      return(TRUE) # All values are NA
    }
    valid_indices <- seq_along(x)[-(1:nas)]
    return(all(!is.na(x[valid_indices])))
  }

  # Case with weights
  nas_x <- sum(is.na(x))
  nas_wts <- sum(is.na(wts))

  valid_x <- if (nas_x > 0) seq_along(x)[-(1:nas_x)] else seq_along(x)
  valid_wts <- if (nas_wts > 0) seq_along(wts)[-(1:nas_wts)] else seq_along(wts)

  common_valid <- intersect(valid_x, valid_wts)

  if (length(common_valid) == 0) {
    return(TRUE) # No valid data points
  }

  # Ensure single logical value return
  all(!is.na(x[common_valid])) && all(!is.na(wts[common_valid]))
}

# Helper function to create test data
create_test_data <- function() {
  data(ttrc) # Load sample financial data
  rownames(ttrc) <- ttrc$Date
  ttrc$Date <- NULL

  list(
    all = ttrc[1:250, ], # Complete data
    top = { # Data with NAs at beginning
      x <- ttrc[1:250, ]
      x[1:10, ] <- NA
      x
    },
    mid = { # Data with NAs in middle
      x <- ttrc[1:250, ]
      x[9:20, ] <- NA
      x
    }
  )
}

# Test WMA with volume weights
test.WMAvol <- function() {
  input <- create_test_data()
  load(system.file("unitTests/output.MA.rda", package = "eTTR"))

  x <- input$all$Close
  wts <- input$all$Volume

  # Calculate WMA - ensure 2D output
  wma_vol <- WMA(x, wts = wts)

  # Convert to matrix if vector
  if (is.vector(wma_vol)) {
    wma_vol <- as.matrix(wma_vol)
  }

  colnames(wma_vol) <- "WMAvol"

  checkEqualsNumeric(
    wma_vol, output$allWMAvol,
    "Volume-weighted WMA calculation error"
  )
  checkEquals(
    colnames(wma_vol), "WMAvol",
    "Volume WMA column name mismatch"
  )
}

# Test column names for xts objects
test.ma.on.xts.objects.have.colnames <- function() {
  input <- create_test_data()

  x <- input$all$Close
  wts <- input$all$Volume

  wma_vol_result <- WMA(x, wts = wts)

  # Ensure 2D output before setting colnames
  if (is.vector(wma_vol_result)) {
    wma_vol_result <- as.matrix(wma_vol_result)
  }

  colnames(wma_vol_result) <- "WMAvol"

  checkEquals(
    colnames(wma_vol_result), "WMAvol",
    "Column names not properly set for WMA results"
  )
}
