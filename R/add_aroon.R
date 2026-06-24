#' @title Aroon Indicator
#' @description
#' Calculates the Aroon indicator for each security in a long-format panel data
#' frame. The Aroon indicator identifies the start of new trends by measuring
#' how recently the highest high (\code{aroonUp}) or lowest low (\code{aroonDn})
#' occurred within the last \code{n} periods. Both lines range from 0 to 100.
#' \code{aroonUp} near 100 signals a strong uptrend; \code{aroonDn} near 100
#' signals a strong downtrend. The oscillator (\code{aroonOsc}) is the
#' difference \code{aroonUp - aroonDn}, ranging from -100 to 100. Developed by
#' Tushar Chande in 1995. Required columns: \code{high}, \code{low}.
#'
#' @param mkt_data A long-format panel data frame or tibble. Must contain
#'   columns \code{date}, \code{code}, \code{high}, and \code{low}.
#' @param n Integer. Look-back window. Defaults to \code{20}.
#' @param append Logical. If \code{TRUE} (default), append result columns to
#'   \code{mkt_data}. If \code{FALSE}, return only \code{date}, \code{code},
#'   \code{name}, and the result columns.
#' @param output Character. \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return A \code{tibble} or \code{data.frame} sorted by \code{date} then
#'   \code{code}, with columns:
#'   \describe{
#'     \item{aroonUp_<n>}{Aroon up line (0 to 100): percentage of periods since
#'       the highest high within the look-back window.}
#'     \item{aroonDn_<n>}{Aroon down line (0 to 100): percentage of periods
#'       since the lowest low within the look-back window.}
#'     \item{aroonOsc_<n>}{Aroon oscillator: \code{aroonUp_<n>} minus
#'       \code{aroonDn_<n>}, ranging from -100 to 100.}
#'   }
#' @export
#' @importFrom xts xts
#' @importFrom tibble as_tibble
#' @importFrom TTR aroon
#' @examples
#' data(ettr_stocks)
#' result <- add_aroon(ettr_stocks)
add_aroon <- function(mkt_data, n = 20, append = TRUE, output = c("tibble", "data.frame")) {
  # ── Argument resolution ────────────────────────────────────────────────────
  output <- match.arg(output)

  # ── Input validation ───────────────────────────────────────────────────────
  if (!inherits(mkt_data, "data.frame")) {
    stop("'mkt_data' must be a long-format data frame with columns: date, code, high, low.")
  }
  required_cols <- c("date", "code", "high", "low")
  missing_cols <- setdiff(required_cols, colnames(mkt_data))
  if (length(missing_cols) > 0) {
    stop(paste0("'mkt_data' is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # ── Split-apply-combine ────────────────────────────────────────────────────
  codes <- unique(mkt_data$code)
  result_list <- lapply(codes, function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]

    # Build HL xts
    hl <- xts::xts(
      cbind(High = sub$high, Low = sub$low),
      order.by = sub$date
    )

    aroon_val <- TTR::aroon(hl, n = n)
    sub[[paste0("aroonUp_", n)]] <- as.numeric(aroon_val[, "aroonUp"])
    sub[[paste0("aroonDn_", n)]] <- as.numeric(aroon_val[, "aroonDn"])
    sub[[paste0("aroonOsc_", n)]] <- as.numeric(aroon_val[, "oscillator"])
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  # ── Optionally drop original columns ──────────────────────────────────────
  if (!append) {
    new_cols <- c(paste0("aroonUp_", n), paste0("aroonDn_", n), paste0("aroonOsc_", n))
    keep <- intersect(c("date", "code", "name", new_cols), colnames(res))
    res <- res[, keep, drop = FALSE]
  }

  # ── Output format ──────────────────────────────────────────────────────────
  if (output == "tibble") tibble::as_tibble(res) else as.data.frame(res, stringsAsFactors = FALSE)
}
