# RUnit tests for Stochastic oscillator
library(RUnit)

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list(all = ttrc[1:250, ], top = ttrc[1:250, ], mid = ttrc[1:250, ])
input$top[1:10, ] <- NA
input$mid[9:20, ] <- NA

# Load output data
load(system.file("unitTests/output.Oscillators.rda", package = "eTTR"))

# Stochastic test suite
test.stoch <- function() {
  suite <- defineTestSuite("stoch",
    testfunctions = list(
      test.stoch,
      test.stoch.for.Inf.fastK
    ),
    dir = "tests"
  )
  return(suite)
}

test.stoch <- function() {
  # Handle data frame to matrix conversion for attributes
  ia <- as.matrix(input$all[, c("High", "Low", "Close")])
  it <- as.matrix(input$top[, c("High", "Low", "Close")])
  oa <- stoch(ia)
  ot <- stoch(it)
  checkEqualsNumeric(oa, output$allStoch, "Stochastic calculation error")
  checkEquals(attributes(oa), attributes(output$allStoch), "Stochastic attributes mismatch")
  checkEqualsNumeric(ot, output$topStoch, "Stochastic with NA input error")
  checkEquals(attributes(ot), attributes(output$topStoch), "Stochastic attributes with NA mismatch")
  checkException(stoch(input$mid[, c("High", "Low", "Close")]), "Stochastic should throw error on partial NA")
}

test.stoch.for.Inf.fastK <- function() {
  a <- c(53.99, 54.69, rep(55.55, 3), rep(52.5, 13), rep(51.77, 2))
  idx <- structure(1446422400 + cumsum(c(
    0, rep(86400, 4), 259200,
    rep(86400, 4), 259200, rep(86400, 4), 259200, rep(86400, 2), 172800,
    259200
  )), tzone = "UTC", tclass = "Date")
  X <- structure(c(a, a, a + 0.1),
    .Dim = c(20L, 3L), class = c("xts", "zoo"),
    index = idx, .Dimnames = list(NULL, c("High", "Low", "Close"))
  )

  o <- structure(
    c(
      rep(NA, 9), rep(0.0327868852459021, 5), rep(0.5, 4),
      rep(0.136986301369856, 2), rep(NA, 11), rep(0.0327868852459021, 3),
      0.188524590163935, 0.344262295081967, 0.5, 0.5, 0.378995433789952,
      0.257990867579904, rep(NA, 13), 0.0327868852459021, 0.084699453551913,
      0.188524590163935, 0.344262295081967, 0.448087431693989,
      0.459665144596651, 0.378995433789952
    ),
    .Dim = c(20L, 3L),
    .Dimnames = list(NULL, c("fastK", "fastD", "slowD")), index = idx,
    class = c("xts", "zoo")
  )

  s <- TTR::stoch(X, 10, 3)
  checkEqualsNumeric(s, o, "Stochastic with Inf fastK error")
}
