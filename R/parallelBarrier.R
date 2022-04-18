#' Price Multiple Barrier Options uUsing Monte Carlo Method and the Parallel Method
#'
#' @description \code{parallelBarrier} is a function enhancing the pricing procedure of a Down (Knock-out) Barrier Option (Call or Put has to be specified in the arguments) evaluation using the Monte Carlo method. This function does not work in windows as the function \code{mcmapply} from the \code{parallel} package is not supported by windows.
#'
#' @param data dataset containing the underlying parameters for the options you want to price. The default set is \code{barrier}.
#' @param Option "Put" or "Call"
#' @param m nbr of price change simulated
#' @param numsim nbr of paths simulated
#' @param InOut "In" if you want the price of a Down-In option or "Out" if you want the price od a Down-Out option.
#' @param nbr.cores number of cores you want to allow the function to use. It's set at 3 cores (\code{3L}) per default, it can be changed.
#'
#' @import parallel
#' @return the output is a vector of the expected prices of the underlying options.
#' @export
#'
parallelBarrier<- function(data=barrier,Option,m,numsim,InOut,nbr.cores=3L){

if (Option == "Call") {
    Price<-parallel::mcmapply(calldown,data[,"UNDER"],data[,"STRIKE"],
                              data[,"VOL"] ,data[,"INTEREST"], data[,"EXPIRY"],
                              data[,"DY"], data[,"BARRIER"],m,numsim,InOut,
                              mc.cores = getOption("mc.cores", nbr.cores))
    }
if (Option == "Put") {
    Price<-parallel::mcmapply(putdown,data[,"UNDER"],data[,"STRIKE"],
                              data[,"VOL"] ,data[,"INTEREST"], data[,"EXPIRY"],
                              data[,"DY"], data[,"BARRIER"],m,numsim, InOut,
                              mc.cores = getOption("mc.cores", nbr.cores))
  }
  return(Price)
}


