#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2007-2013  Deng Yishuo
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#' @title Sliding Window Linear Regression
#' @description
#' Performs linear regression over a sliding window on a time series,
#' providing coefficients and statistical metrics for each window.
#'
#' @param x A time series (vector or xts object).
#' @param n The size of the sliding window (number of periods for regression). Default is 14.
#' @param include.intercept Logical indicating whether to include an intercept term. Default is TRUE.
#' @param output The type of output: "all" returns all regression results, "slope" returns only the slope.
#' @return If output is "slope", returns a vector or xts object containing slope values.
#'         If output is "all", returns a list of class "runRegression" containing coefficients,
#'         R-squared, adjusted R-squared, F-statistic, and p-value.
#' @details
#' This function computes linear regression over a sliding window of specified size.
#' It is particularly useful for analyzing trends and momentum in financial time series.
#' @importFrom xts xts
#' @importFrom stats lm coef pf setNames
#' @export
#' @examples
#' # Generate sample data
#' set.seed(123)
#' x <- xts::xts(rnorm(100), order.by = Sys.Date() - 99:0)
#' # Calculate slope-only results
#' slopes <- runRegression(x, n = 10, output = "slope")
#' # Calculate full regression results
#' reg_results <- runRegression(x, n = 10, output = "all")
runRegression <- function(x, n = 14, include.intercept = TRUE, output = "all") {
  # Input validation
  if (!is.numeric(x)) {
    stop("Input x must be a numeric vector or xts object")
  }

  if (length(x) < n) {
    stop("Input length must be greater than window size n")
  }

  if (n <= 1) {
    stop("Window size n must be greater than 1")
  }

  if (!is.logical(include.intercept)) {
    stop("include.intercept must be a logical value (TRUE or FALSE)")
  }

  if (!output %in% c("all", "slope")) {
    stop("output parameter must be 'all' or 'slope'")
  }

  # Initialize result matrix
  result_length <- length(x) - n + 1

  # Set coefficient names
  coef_names <- if (include.intercept) {
    c("intercept", paste0("slope_", n, "d"))
  } else {
    paste0("slope_", n, "d")
  }

  coefficients <- matrix(NA, nrow = result_length, ncol = length(coef_names))
  colnames(coefficients) <- coef_names

  # Create time index (if input is xts)
  if (inherits(x, "xts")) {
    result_index <- index(x)[n:length(x)]
  } else {
    result_index <- n:length(x)
  }

  # Create x variable (time index)
  x_idx <- 1:n

  # Preallocate result vectors
  r_squared <- numeric(result_length)
  adj_r_squared <- numeric(result_length)
  f_statistic <- numeric(result_length)
  p_value <- numeric(result_length)

  # Efficiently compute sliding window regression
  for (i in 1:result_length) {
    # Get data for current window
    window_data <- x[i:(i + n - 1)]

    # Build model formula
    if (include.intercept) {
      model <- lm(window_data ~ x_idx)
    } else {
      model <- lm(window_data ~ x_idx - 1)
    }

    # Extract regression coefficients
    coefficients[i, ] <- coef(model)

    # Extract goodness of fit
    summary_model <- summary(model)
    r_squared[i] <- summary_model$r.squared
    adj_r_squared[i] <- summary_model$adj.r.squared

    # Extract F-statistic and p-value
    f_statistic[i] <- summary_model$fstatistic[1]
    p_value[i] <- pf(f_statistic[i], summary_model$fstatistic[2],
      summary_model$fstatistic[3],
      lower.tail = FALSE
    )
  }

  # Determine return content based on output parameter
  if (output == "slope") {
    # Return only slope
    slope_col <- if (include.intercept) 2 else 1
    result <- coefficients[, slope_col, drop = FALSE]

    if (inherits(x, "xts")) {
      result <- xts(result, order.by = result_index)
      colnames(result) <- coef_names[slope_col]
    }

    return(result)
  } else {
    # Return all results
    result <- list(
      coefficients = if (inherits(x, "xts")) {
        xts::xts(coefficients, order.by = result_index)
      } else {
        coefficients
      },
      r.squared = if (inherits(x, "xts")) {
        xts(r_squared, order.by = result_index, name = paste0("r_squared_", n, "d"))
      } else {
        setNames(r_squared, paste0("r_squared_", n, "d"))
      },
      adj.r.squared = if (inherits(x, "xts")) {
        xts(adj_r_squared, order.by = result_index, name = paste0("adj_r_squared_", n, "d"))
      } else {
        setNames(adj_r_squared, paste0("adj_r_squared_", n, "d"))
      },
      f.statistic = if (inherits(x, "xts")) {
        xts(f_statistic, order.by = result_index, name = paste0("f_stat_", n, "d"))
      } else {
        setNames(f_statistic, paste0("f_stat_", n, "d"))
      },
      p.value = if (inherits(x, "xts")) {
        xts(p_value, order.by = result_index, name = paste0("p_value_", n, "d"))
      } else {
        setNames(p_value, paste0("p_value_", n, "d"))
      }
    )

    class(result) <- "runRegression"
    return(result)
  }
}

#' @title Print Method for runRegression Objects
#' @description Prints a summary of the sliding window regression results.
#' @param x A runRegression object.
#' @param ... Additional arguments passed to print.
#' @return The input object is returned invisibly.
#' @importFrom xts xts
#' @importFrom utils tail
#' @export
#' @method print runRegression
#' @examples
#' # Generate sample data and run regression
#' set.seed(123)
#' x <- xts::xts(rnorm(100), order.by = Sys.Date() - 99:0)
#' reg_results <- runRegression(x, n = 10)
#' print(reg_results)
print.runRegression <- function(x, ...) {
  cat("Sliding Window Linear Regression Results (Window Size:", nrow(x$coefficients) + 1, ")\n")
  cat("Regression Terms:", paste(colnames(x$coefficients), collapse = ", "), "\n")
  cat("Number of Samples:", nrow(x$coefficients), "\n")

  # Display summary of R-squared statistics
  r2_mean <- mean(x$r.squared, na.rm = TRUE)
  r2_min <- min(x$r.squared, na.rm = TRUE)
  r2_max <- max(x$r.squared, na.rm = TRUE)
  cat(sprintf("R-squared Statistics: Mean=%.4f, Min=%.4f, Max=%.4f\n", r2_mean, r2_min, r2_max))

  # Display regression coefficients
  cat("\nRegression Coefficients:\n")
  print((x$coefficients))
  invisible(x)
}

#' @title Convert runRegression Results to Data Frame
#' @description
#' Converts the results of a runRegression analysis into a data frame for easier manipulation.
#'
#' @param reg_result A runRegression object.
#' @param prefix An optional prefix to add to column names.
#'
#' @return A data frame containing regression results.
#' @export
#' @examples
#' # Generate sample data and run regression
#' set.seed(123)
#' x <- xts::xts(rnorm(100), order.by = Sys.Date() - 99:0)
#' reg_results <- runRegression(x, n = 10)
#' # Convert to data frame
#' df <- convertRegressionToDataframe(reg_results, prefix = "reg_10d")
convertRegressionToDataframe <- function(reg_result, prefix = "") {
  # Extract coefficients
  coef_df <- as.data.frame(reg_result$coefficients)
  coef_df$date <- zoo::index(reg_result$coefficients)

  # Extract other metrics
  metrics_df <- data.frame(
    date = index(reg_result$r.squared),
    r_squared = as.numeric(reg_result$r.squared),
    adj_r_squared = as.numeric(reg_result$adj.r.squared),
    f_statistic = as.numeric(reg_result$f.statistic),
    p_value = as.numeric(reg_result$p.value)
  )

  # Merge all data
  result_df <- merge(coef_df, metrics_df, by = "date")

  # Add prefix (if provided)
  if (prefix != "") {
    colnames(result_df)[-1] <- paste0(prefix, "_", colnames(result_df)[-1])
  }

  # Sort by date
  result_df <- result_df[order(result_df$date), ]

  return(result_df)
}
