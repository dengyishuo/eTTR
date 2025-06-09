# DEMA Test File

# Safe NA check function (only needed for DEMA tests)
safe_na_check <- function(x, wts = NULL) {
  n <- length(x)
  if (n == 0) {
    return(TRUE)
  }

  # Handle case without weights
  if (is.null(wts)) {
    non_na <- which(!is.na(x))
    if (length(non_na) == 0) {
      return(TRUE)
    }

    first_valid <- min(non_na)
    if (first_valid == n) {
      return(FALSE)
    }

    return(any(is.na(x[(first_valid + 1):n])))
  }

  # Handle case with weights
  if (length(x) != length(wts)) {
    stop("x and wts must have the same length")
  }

  non_na_x <- which(!is.na(x))
  non_na_wts <- which(!is.na(wts))

  if (length(non_na_x) == 0 || length(non_na_wts) == 0) {
    return(TRUE)
  }

  first_x <- min(non_na_x)
  first_wts <- min(non_na_wts)
  common_start <- max(first_x, first_wts)

  if (common_start == n) {
    return(FALSE)
  }

  idx <- (common_start + 1):n
  return(any(is.na(x[idx])) || any(is.na(wts[idx])))
}

# Load test data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

# Create test inputs
input <- list(all = ttrc[1:250, ], top = ttrc[1:250, ], mid = ttrc[1:250, ])
input$top[1:10, ] <- NA
input$mid[9:20, ] <- NA

# Load expected outputs
load(system.file("unitTests/output.MA.rda", package = "eTTR"))

# Test cases for DEMA
test.DEMA <- function() {
  checkEqualsNumeric(DEMA(input$all$Close), output$allDEMA, "DEMA calculation error")
  checkEquals(attributes(DEMA(input$all$Close)), attributes(output$allDEMA), "DEMA attributes mismatch")
  checkEqualsNumeric(DEMA(input$top$Close), output$topDEMA, "DEMA with leading NA error")
  checkEquals(attributes(DEMA(input$top$Close)), attributes(output$topDEMA), "DEMA attributes with NA mismatch")

  # For non-leading NA, expect all NA result
  result <- try(DEMA(input$mid$Close), silent = TRUE)
  checkTrue(
    inherits(result, "try-error") || all(is.na(result)),
    "DEMA should error or return all NA on non-leading NA"
  )

  checkException(DEMA(input$all[, 1:2]), "DEMA should reject multi-column input")
}
