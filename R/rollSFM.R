#' Rolling Single Factor Model
#'
#' Estimate rolling single-factor linear regression metrics over a fixed window: alpha, beta, and R-squared.
#' All calculations rely on fast rolling summary utility functions provided by this package.
#'
#' @param Ra xts object, return series of the asset to be modeled (dependent variable).
#' @param Rb xts object, factor benchmark return series (independent explanatory variable).
#' @param n Integer rolling window length, default 60 periods.
#'
#' @details
#' The single factor model formula:
#' \deqn{R_a = \alpha + \beta R_b + \epsilon}
#'
#' Computation steps:
#' 1. Rolling beta: covariance of asset & factor divided by factor variance
#' \deqn{\beta = \frac{runCov(R_a, R_b, n)}{runVar(R_b, n)}}
#' 2. Rolling alpha: de-meaned asset return minus beta times de-meaned factor return
#' \deqn{\alpha = runMean(R_a, n) - \beta \times runMean(R_b, n)}
#' 3. Rolling R-squared: 1 minus ratio of residual variance to asset total variance
#'
#' All intermediate rolling statistics use internal compiled rolling functions for performance.
#' Output is a merged xts object aligned to the input index of \code{Ra}.
#'
#' @return xts object with three columns:
#' \itemize{
#'   \item \code{alpha}: Rolling single-factor intercept
#'   \item \code{beta}: Rolling single-factor slope coefficient
#'   \item \code{r.squared}: Rolling model R-squared goodness-of-fit
#' }
#'
#' @importFrom xts merge.xts
#' @export
rollSFM <- function(Ra, Rb, n = 60) {
  # Calculate rolling single-factor model coefficients and fit statistics

  # Rolling beta coefficient
  beta <- runCov(Ra, Rb, n) / runVar(Rb, n = n)

  # Rolling alpha intercept
  alpha <- runMean(Ra, n) - beta * runMean(Rb, n)

  # Rolling residual variance
  se.resid <-
    1 / (n * (n - 2)) * (n * runSum(Ra^2, n) - runSum(Ra, n)^2
      - beta^2 * (n * runSum(Rb^2, n) - runSum(Rb, n)^2))

  # Rolling total variance of asset returns
  se.Ra <- runVar(Ra, n = n) * (n - 1) / (n - 2)

  # Rolling R-squared
  r.squared <- 1 - se.resid / se.Ra

  result <- merge(alpha, beta, r.squared)
  return(result)
}
