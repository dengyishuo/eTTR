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
#' @title Align an object with a target index
#' @description
#' Ensures that the input object is aligned with the specified target index,
#' filling missing values with NA if necessary.
#'
#' @param x The input object (numeric vector or xts).
#' @param target_index The target index to align with.
#' @param fill_value The value to use for filling missing positions, default is NA.
#' @return An xts object aligned with the target index.
#' @importFrom zoo index
#' @keywords internal
#' @export
align_with_index <- function(x, target_index, fill_value = NA) {
  if (is.null(target_index)) {
    return(x)
  }

  # If x is already xts and length matches, return directly
  if (is.xts(x) && length(zoo::index(x)) == length(target_index)) {
    return(x)
  }

  # If x is not xts, convert to xts
  if (!is.xts(x)) {
    x_xts <- xts(rep(fill_value, length(target_index)), order.by = target_index)
    if (length(x) > 0) {
      x_xts[(length(target_index) - length(x) + 1):length(target_index)] <- x
    }
    return(x_xts)
  }

  # If x is xts but length doesn't match, extend or truncate
  current_length <- length(zoo::index(x))
  target_length <- length(target_index)

  if (current_length == target_length) {
    # Same length but indices might differ, reset index
    zoo::index(x) <- target_index
    return(x)
  } else if (current_length < target_length) {
    # Extend and fill with NA
    new_x <- xts(rep(fill_value, target_length), order.by = target_index)
    new_x[(target_length - current_length + 1):target_length] <- coredata(x)
    return(new_x)
  } else {
    # Truncate
    return(head(x, target_length))
  }
}
