#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2025 - 2030  DengYishuo
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
#' Generate trading signals based on technical indicators (tibble output)
#' @description This function generates trading signals (-1, 0, 1) based on specified columns
#' of a technical indicator matrix, returning tibble format for consistent data handling.
#' Supports column parameters as vectors for flexible column selection.
#' @param indicator_data Tibble or data frame containing technical indicator columns
#' @param method Character specifying signal generation method: "threshold", "crossover", or "comparison"
#' @param col1 Character vector of column names or numeric vector of indices for the first set of columns
#' @param col2 Character vector of column names or numeric vector of indices for the second set of columns (for crossover/comparison)
#' @param threshold Numeric threshold value (required for "threshold" method)
#' @param threshold_type Character specifying threshold type: "above", "below", or "outside"
#' @param append Logical indicating whether to append signals to the original data (default TRUE)
#' @param signal_name Character specifying the name of the signal column (default "Signal")
#' @return Tibble with trading signals added or standalone signal tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
#' @examples
#' \dontrun{
#' # Example usage with tibble output
#' data(TSLA) # Load example data
#' kdj_tibble <- add_kdj(TSLA)
#' signal_tibble <- gen_Sig(kdj_tibble, method = "crossover", col1 = "K", col2 = "D")
#' class(signal_tibble) # Should return "tbl_df", "tbl", "data.frame"
#' }
gen_Sig <- function(indicator_data,
                    method = c("threshold", "crossover", "comparison"),
                    col1,
                    col2 = NULL,
                    threshold = NULL,
                    threshold_type = c("above", "below", "outside"),
                    append = TRUE,
                    signal_name = "Signal") {
  # Validate input types
  method <- match.arg(method)
  threshold_type <- match.arg(threshold_type)

  # Convert input to tibble for consistent handling
  if (!inherits(indicator_data, "tbl")) {
    indicator_data <- tibble::as_tibble(indicator_data)
  }

  # Helper function: process column vector input
  process_col_vector <- function(col_ref) {
    if (is.null(col_ref)) {
      return(NULL)
    }

    # Ensure input is a vector
    if (!is.vector(col_ref)) {
      col_ref <- as.vector(col_ref)
    }

    # Convert column names/indices to column index vector
    col_indices <- vector("integer", length(col_ref))
    for (i in seq_along(col_ref)) {
      ref <- col_ref[i]
      if (is.numeric(ref)) {
        if (ref < 1 || ref > ncol(indicator_data)) {
          stop(sprintf("Column index %d out of bounds", ref))
        }
        col_indices[i] <- as.integer(ref)
      } else if (is.character(ref)) {
        if (!ref %in% colnames(indicator_data)) {
          stop(sprintf("Column '%s' not found in indicator_data", ref))
        }
        col_indices[i] <- which(colnames(indicator_data) == ref)
      } else {
        stop("Column references must be numeric indices or character column names")
      }
    }
    return(unique(col_indices)) # Deduplicate
  }

  # Process column vector input
  col1_indices <- process_col_vector(col1)
  col2_indices <- if (!is.null(col2)) process_col_vector(col2) else NULL

  # Validate parameters based on method
  if (method %in% c("crossover", "comparison")) {
    if (is.null(col2_indices)) {
      stop("col2 is required for 'crossover' and 'comparison' methods")
    }

    # Check column count matching
    if (length(col1_indices) != length(col2_indices)) {
      stop("col1 and col2 must have the same number of columns for crossover/comparison")
    }
  }

  if (method == "threshold" && is.null(threshold)) {
    stop("threshold is required for 'threshold' method")
  }

  # Initialize signal vector
  signals <- rep(0, nrow(indicator_data))

  # Signal generation logic (support multiple columns)
  if (method == "threshold") {
    for (col_idx in col1_indices) {
      col_data <- indicator_data[[col_idx]]

      if (threshold_type == "above") {
        signals[col_data > threshold] <- 1
        signals[col_data < threshold] <- -1
      } else if (threshold_type == "below") {
        signals[col_data < threshold] <- 1
        signals[col_data > threshold] <- -1
      } else if (threshold_type == "outside") {
        signals[col_data > threshold | col_data < -threshold] <- 1
        signals[col_data <= threshold & col_data >= -threshold] <- -1
      }
    }
  } else if (method == "crossover") {
    for (i in seq_along(col1_indices)) {
      col1_data <- indicator_data[[col1_indices[i]]]
      col2_data <- indicator_data[[col2_indices[i]]]
      diff_data <- col1_data - col2_data

      for (t in 2:nrow(indicator_data)) {
        if (!is.na(diff_data[t]) && !is.na(diff_data[t - 1])) {
          if (diff_data[t] > 0 && diff_data[t - 1] < 0) {
            signals[t] <- 1 # Golden cross
          } else if (diff_data[t] < 0 && diff_data[t - 1] > 0) {
            signals[t] <- -1 # Death cross
          }
        }
      }
    }
  } else if (method == "comparison") {
    for (i in seq_along(col1_indices)) {
      col1_data <- indicator_data[[col1_indices[i]]]
      col2_data <- indicator_data[[col2_indices[i]]]

      signals[col1_data > col2_data] <- 1
      signals[col1_data < col2_data] <- -1
    }
  }

  # Create signal tibble
  signal_tibble <- tibble::as_tibble(data.frame(signals))
  colnames(signal_tibble) <- signal_name

  # Return result as tibble
  if (append) {
    return(dplyr::bind_cols(indicator_data, signal_tibble))
  } else {
    return(signal_tibble)
  }
}
