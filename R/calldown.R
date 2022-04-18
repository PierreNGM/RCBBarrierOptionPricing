#' Price Call Down Barrier Option using Monte Carlo method
#'
#' @description \code{calldown} is a fuction enhancing the pricinge precedure of a Call Down (Knock-out) Barrier Option evaluation using Monte Carlo method.
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
calldown<-function(s, k, v, r, tt, d, B,  m, numsim, InOut="In"){

  S <- SPrice(s, k, v, r, tt, d, m, numsim)
  ST     <- S[, m]
  ST_min <- apply(S, 1, FUN = min, na.rm = TRUE)

  avgpricecalldownin  <- mean(ifelse(ST_min < B,
                                     pmax(  ST - k ,0) , 0)) * exp(-r * tt)
  avgpricecalldownout <- mean(ifelse(ST_min > B,
                                     pmax(  ST - k ,0) , 0)) * exp(-r * tt)

  if (InOut=="In")  return(avgpricecalldownin)
  if (InOut=="Out") return(avgpricecalldownout)
}
