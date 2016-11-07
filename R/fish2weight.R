#' Calculate weight from fish age
#'
#' Wrapper function for \code{\link{age2length}} and \code{\link{length2weight}} to estimate weight in each cell from \code{fish}.
#'
#' @param fish
#' @param ages The ages of the fish of the fish to be converted to weight (year)
#' @param l_to_w_int
#' @param l_to_w_power
#' @param Linf_mean
#' @param Linf_SD
#' @param k_mean
#' @param k_SD
#' @param t0
#' @inheritParams length2weight
#' @inheritParams age2length
#' @inheritParams dispersal
#'
#' @return
#' @export
#'
#' @examples
#' # create a fish matrix
#' fish <- initpop(initial_abun=250*10^6,cells=length(BESTMPA_domain),maxage=50,rate=0.7)
#' # calculate weight
#' fish2weight(fish,ages=0:20,l_to_w_int=0.000011,l_to_w_power=2.91,Linf_mean=112.03,Linf_SD=10.46/1.96,k_mean=0.13,k_SD=0.021/196,t0=0.18)
fish2weight <- function(fish,ages,l_to_w_int=0.000011,l_to_w_power=2.91,Linf_mean=112.03,Linf_SD=10.46/1.96,k_mean=0.13,k_SD=0.021/196,t0=0.18){
    apply(fish[,ages+1],1,function(x) sum(length2weight(age2length(ages))*x))
}
