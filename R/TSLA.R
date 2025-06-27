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
#' Tesla Inc. Stock Price Data
#'
#' Daily stock prices for Tesla Inc. (TSLA) from 2010 to present.
#' @name TSLA
#' @docType data
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{Open}{Opening price}
#'   \item{High}{Highest price}
#'   \item{Low}{Lowest price}
#'   \item{Close}{Closing price}
#'   \item{Volume}{Trading volume}
#' }
#' @source Yahoo Finance
#' @keywords TSLA
#' @examples
#' data(TSLA)
#' plot(tail(TSLA[, "Close"], 100), type = "l")
#' @rdname TSLA
NULL
