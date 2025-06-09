#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2025-2030  DengYishuo
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
#' @title NA Check
#' @description Validates that NA values are only present at the start of a series.
#'
#' @param x Object that is coercible to xts or matrix.
#' @param n Starting position offset (default is 0).
#'
#' @return An invisible list containing:
#'   \item{NAs}{Number of leading NA values.}
#'   \item{nonNA}{Indices of non-NA values.}
#'   \item{beg}{Starting index after accounting for NAs and offset.}
#'
#' @export
#'
#' @examples
#' data <- c(NA, NA, 3, 4, 5)
#' naCheck(data)
#'
#' @author DengYishuo
#' @keywords ts
#'
naCheck <- function(x, n = 0) {
  # Ensure NAs are only at beginning of data.
  if (is.null(dim(x)[2])) {
    NAs <- sum(is.na(x))
    if (NAs > 0) {
      if (any(is.na(x[-(1:NAs)]))) stop("Series contains non-leading NAs")
    }
  } else {
    NAs <- sum(rowSums(is.na(x)) > 0)
    if (NAs > 0) {
      if (any(is.na(x[-(1:NAs), ]))) stop("Series contains non-leading NAs")
    }
  }

  res <- list()
  res$NAs <- NAs
  res$nonNA <- (1 + NAs):NROW(x)
  res$beg <- n + NAs

  invisible(res)
}
