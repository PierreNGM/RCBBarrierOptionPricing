#' Simulate multiple stock prices
#'
#' @description \code{SPrice} is a function to simulate multiple stock prices with specific characteristics.
#'
#' @param s underlying
#' @param k strike
#' @param v volatility
#' @param r riskFreeRate
#' @param tt maturity
#' @param d dividendYield
#' @param m nbr of price change simulated
#' @param numsim nbr of paths simulated
#'
#' @return the output is a matrix where each row is a different stock (1:numsim), and each column is another time point (1:m).
#' @import stats
#' @export
#'
#'
SPrice <- function (s, k, v, r, tt, d, m, numsim)
{
  z <- matrix(rnorm(m * numsim), numsim, m)
  zcum <- t(apply(z, 1, cumsum))
  h <- tt/m
  hmat <- matrix((1:m) * h, numsim, m, byrow = TRUE)
  S <- matrix(0, nrow = numsim, ncol = m)
  for (i in 1:m) {
    S[, i] <- s * exp((r - d + 0.5 * v^2) * h * i + v * sqrt(h) * zcum[, i])
  }
  return(S)
}

