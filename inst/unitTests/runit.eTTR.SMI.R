library(RUnit)
library(xts)

# Enhanced numeric validation function
validate.numeric.operations <- function(price_data) {
  # Convert columns to numeric if needed
  price_data$High <- as.numeric(price_data$High)
  price_data$Low <- as.numeric(price_data$Low)

  # Check for NA conversion issues
  if (any(is.na(price_data$High)) && !all(is.na(price_data$High))) {
    warning("Non-numeric values found in High column, converted to NA")
  }
  if (any(is.na(price_data$Low)) && !all(is.na(price_data$Low))) {
    warning("Non-numeric values found in Low column, converted to NA")
  }

  # Calculate rolling max/min without na.rm argument
  n <- 14
  hmax <- runMax(price_data$High, n) # Removed na.rm = FALSE
  lmin <- runMin(price_data$Low, n) # Removed na.rm = FALSE

  # Replace Inf/-Inf with NA if needed
  hmax[is.infinite(hmax)] <- NA
  lmin[is.infinite(lmin)] <- NA

  # Validate before subtraction
  if (!all(is.numeric(hmax), is.numeric(lmin))) {
    stop("Non-numeric values in hmax or lmin calculations")
  }

  price_range <- hmax - lmin
  return(price_range)
}

# Modified test cases with local input data
test.SMI <- function() {
  # Create test data locally within the function
  data(ttrc)
  rownames(ttrc) <- ttrc$Date
  ttrc$Date <- NULL

  input <- list(
    all = ttrc[1:250, ],
    top = ttrc[1:250, ],
    mid = ttrc[1:250, ]
  )
  input$top[1:10, ] <- NA
  input$mid[9:20, ] <- NA

  price_cols <- c("High", "Low", "Close")

  # Test case 1: Valid numeric data
  price_data_all <- input$all[, price_cols]
  range_all <- validate.numeric.operations(price_data_all)
  checkTrue(all(is.numeric(range_all)), "Case 1: Numeric validation failed")

  # Test case 2: Leading NA data
  price_data_top <- input$top[, price_cols]
  range_top <- validate.numeric.operations(price_data_top)
  checkTrue(all(is.na(range_top[1:10])), "Case 2: Leading NA handling failed")

  # Test case 3: Non-leading NA data
  price_data_mid <- input$mid[, price_cols]
  checkException(
    validate.numeric.operations(price_data_mid),
    "Case 3: Should throw error for non-leading NAs"
  )
}
