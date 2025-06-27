#' Enhanced Technical Trading Rule Composite data
#'
#' Historical Open, High, Low, Close, and Volume data for the periods January 2,
#' 1985 to December 31, 2006. Randomly generated.
#'
#' These data do not represent an actual security.  They are provided so
#' examples do not necessitate an internet connection.
#'
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
#' @name ttrc
#' @docType data
#' @format The format is: \tabular{lll}{
#'   Date: \tab Class 'Date' \tab 5480 5481 5482 5485 5486 ...\cr
#'   Open: \tab num \tab 3.18 3.09 3.11 3.09 3.10 ...\cr
#'   High: \tab num \tab 3.18 3.15 3.12 3.12 3.12 ...\cr
#'   Low: \tab num \tab 3.08 3.09 3.08 3.07 3.08 ...\cr
#'   Close: \tab num \tab 3.08 3.11 3.09 3.10 3.11 ...\cr
#'   Volume: \tab num \tab 1870906 3099506 2274157 2086758 2166348 ...\cr
#' }
#' @source Randomly generated.
#' @keywords datasets
#' @examples
#' data(ttrc)
#' plot(tail(ttrc[, "Close"], 100), type = "l")
#' @rdname ttrc
NULL
