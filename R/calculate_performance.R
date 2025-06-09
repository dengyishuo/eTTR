#' Calculate Performance Metrics for Trading Strategy
#'
#' Computes various performance metrics for a trading strategy, including return statistics,
#' risk metrics, and trade analysis. This function fixes previous trade pairing issues.
#'
#' @param returns Numeric vector of daily returns. The first day is typically removed
#'                as it often represents a baseline with zero return.
#' @param trades Data frame of trade records with the following columns:
#' \describe{
#'   \item{\code{type}}{Trade type ("buy" or "sell").}
#'   \item{\code{date}}{Trade execution date.}
#'   \item{\code{value}}{Trade value (e.g., price * quantity).}
#'   \item{\code{fee}}{Transaction fee.}
#'   \item{\code{holding_days}}{Days held for the position (only for sell trades).}
#'   \item{\code{exit_reason}}{Reason for exiting the position (only for sell trades).}
#' }
#'
#' @return A list containing the following performance metrics:
#' \describe{
#'   \item{\code{total_return}}{Cumulative return over the entire period.}
#'   \item{\code{annualized_return}}{Annualized return based on total return.}
#'   \item{\code{mean_daily_return}}{Average daily return.}
#'   \item{\code{volatility}}{Standard deviation of daily returns.}
#'   \item{\code{sharpe_ratio}}{Sharpe ratio assuming risk-free rate of 0.}
#'   \item{\code{max_drawdown}}{Maximum drawdown over the period.}
#'   \item{\code{var_95}}{95% Value at Risk.}
#'   \item{\code{es_95}}{95% Expected Shortfall.}
#'   \item{\code{n_trades}}{Number of completed trades (buy-sell pairs).}
#'   \item{\code{win_rate}}{Percentage of profitable trades.}
#'   \item{\code{avg_holding_days}}{Average holding period for trades.}
#'   \item{\code{max_holding_days}}{Maximum holding period for trades.}
#'   \item{\code{exit_reason_counts}}{Table of exit reasons for trades.}
#' }
#'
#' @details
#' This function calculates key performance indicators for a trading strategy. It handles
#' both return-based metrics (e.g., Sharpe ratio, drawdown) and trade-specific metrics
#' (e.g., win rate, holding periods). The trade pairing logic has been improved to ensure
#' each buy is matched with the correct subsequent sell.
#'
#' @seealso \code{\link[PerformanceAnalytics]{Return.annualized}},
#' \code{\link[PerformanceAnalytics]{maxDrawdown}}
#'
#' @importFrom stats sd quantile
#' @importFrom utils head tail
#' @importFrom PerformanceAnalytics maxDrawdown Return.annualized
#'
#' @export
calculate_performance <- function(returns, trades) {
  # Validate input
  if (!is.numeric(returns)) {
    stop("returns must be a numeric vector")
  }

  if (!is.data.frame(trades)) {
    stop("trades must be a data frame")
  }

  # Ensure trades are sorted by date
  if (nrow(trades) > 0) {
    trades <- trades[order(trades$date), ]
  }

  # Remove first day (return is 0)
  valid_returns <- returns[-1]

  # Total return
  total_return <- prod(1 + valid_returns) - 1

  # Annualized return
  n_days <- length(valid_returns)
  annualized_return <- if (n_days > 0) {
    (1 + total_return)^(252 / n_days) - 1
  } else {
    0
  }

  # Daily mean return
  mean_daily_return <- mean(valid_returns)

  # Daily standard deviation (volatility)
  volatility <- sd(valid_returns)

  # Sharpe ratio (risk-free rate assumed to be 0)
  sharpe_ratio <- if (volatility > 0) {
    mean_daily_return / volatility * sqrt(252)
  } else {
    0
  }

  # Maximum drawdown
  cum_returns <- cumsum(valid_returns)
  cum_equity <- exp(cum_returns)
  running_max <- cummax(cum_equity)
  drawdown <- (cum_equity / running_max) - 1
  max_drawdown <- min(drawdown)

  # VaR (Value at Risk) - 95% confidence level
  var_95 <- -quantile(valid_returns, 0.05, na.rm = TRUE)

  # ES (Expected Shortfall) - 95% confidence level
  loss_threshold <- quantile(valid_returns, 0.05, na.rm = TRUE)
  losses <- valid_returns[valid_returns <= loss_threshold]
  es_95 <- if (length(losses) > 0) {
    -mean(losses)
  } else {
    0
  }

  # Trade metrics
  if (nrow(trades) > 0) {
    buy_trades <- trades[trades$type == "buy", ]
    sell_trades <- trades[trades$type == "sell", ]

    if (nrow(buy_trades) > 0 && nrow(sell_trades) > 0) {
      # Match buy and sell trades (improved logic)
      trade_pairs <- list()
      buy_idx <- 1
      sell_idx <- 1

      while (buy_idx <= nrow(buy_trades) && sell_idx <= nrow(sell_trades)) {
        buy_date <- buy_trades$date[buy_idx]
        sell_date <- sell_trades$date[sell_idx]

        if (buy_date < sell_date) {
          trade_pairs[[length(trade_pairs) + 1]] <- list(
            buy = buy_trades[buy_idx, ],
            sell = sell_trades[sell_idx, ]
          )
          buy_idx <- buy_idx + 1
          sell_idx <- sell_idx + 1
        } else {
          sell_idx <- sell_idx + 1
        }
      }

      # Calculate trade statistics
      if (length(trade_pairs) > 0) {
        trade_returns <- numeric()
        holding_days <- numeric()
        exit_reasons <- character()

        for (i in 1:length(trade_pairs)) {
          buy <- trade_pairs[[i]]$buy
          sell <- trade_pairs[[i]]$sell
          net_return <- sell$value - buy$value - buy$fee - sell$fee
          trade_return <- net_return / buy$value
          trade_returns <- c(trade_returns, trade_return)
          holding_days <- c(holding_days, sell$holding_days)
          exit_reasons <- c(exit_reasons, sell$exit_reason)
        }

        win_rate <- mean(trade_returns > 0) * 100
        avg_holding_days <- mean(holding_days, na.rm = TRUE)
        max_holding_days <- max(holding_days, na.rm = TRUE)
        n_trades <- length(trade_returns)
        exit_reason_counts <- table(exit_reasons)
      } else {
        win_rate <- 0
        avg_holding_days <- 0
        max_holding_days <- 0
        n_trades <- 0
        exit_reason_counts <- NULL
      }
    } else {
      win_rate <- 0
      avg_holding_days <- 0
      max_holding_days <- 0
      n_trades <- 0
      exit_reason_counts <- NULL
    }
  } else {
    win_rate <- 0
    avg_holding_days <- 0
    max_holding_days <- 0
    n_trades <- 0
    exit_reason_counts <- NULL
  }

  # Compile results
  list(
    total_return = total_return,
    annualized_return = annualized_return,
    mean_daily_return = mean_daily_return,
    volatility = volatility,
    sharpe_ratio = sharpe_ratio,
    max_drawdown = max_drawdown,
    var_95 = var_95,
    es_95 = es_95,
    n_trades = n_trades,
    win_rate = win_rate,
    avg_holding_days = avg_holding_days,
    max_holding_days = max_holding_days,
    exit_reason_counts = exit_reason_counts
  )
}
