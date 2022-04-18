#' Price Put Down Barrier Option using Monte Carlo method
#'
#' @description \code{putdown} is a fuction enhancing the pricinge precedure of a Put Down (Knock-out) Barrier Option evaluation using Monte Carlo method.
#'
#' @param s = underlying
#' @param k = strike
#' @param v = volatility
#' @param r = riskFreeRate
#' @param tt = maturity
#' @param d = dividendYield
#' @param B = barrier
#' @param m = nbr of price change simulated
#' @param numsim = nbr of paths simulated
#' @param InOut = "In" if you want the price of a Down-In option or "Out" if you want the price od a Down-Out option.
#'
#' @return the output is the expected price for the underlying option.
#' @export
#'
putdown<-function(s, k, v, r, tt, d, B, m, numsim, InOut ="In"){

  S <- SPrice(s, k, v, r, tt, d, m, numsim)
  ST     <- S[, m]
  ST_min <- apply(S, 1, FUN = min, na.rm = TRUE)


  avgpriceputdownin  <- mean(ifelse(ST_min < B,
                                    pmax(  k - ST  ,0) , 0)) * exp(-r * tt)
  avgpricecputdownout <- mean(ifelse(ST_min > B,
                                     pmax(  k - ST ,0) , 0)) * exp(-r * tt)

  if (InOut=="In")  return(avgpriceputdownin)
  if (InOut=="Out") return(avgpricecputdownout)
}


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
