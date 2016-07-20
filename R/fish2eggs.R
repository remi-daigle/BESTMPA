#' Title
#'
#' @param fish
#' @param fecundity
#' @param age_mat_steepness
#' @param age_mat_sigmoid
#' @param l_to_w_int
#' @param l_to_w_power
#' @param Linf_mean
#' @param Linf_SD
#' @param k_mean
#' @param k_SD
#' @param t0
#' @inheritParams age2length
#' @inheritParams length2weight
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
fish2eggs <- function(fish,fecundity=0.5*10^6,age_mat_steepness=2.5,age_mat_sigmoid=4,l_to_w_int=0.000011,l_to_w_power=2.91,Linf_mean=112.03,Linf_SD=10.46/1.96,k_mean=0.13,k_SD=0.021/196,t0=0.18){
    ages <- 1:ncol(fish)-1
    sexually_mature <- 1/(1+exp(-age_mat_steepness*(ages-age_mat_sigmoid)))
    egg_producers <- t(roundprob(apply(fish,1,function(x) sexually_mature*x/2)))
    eggs <- apply(egg_producers,1,function(x) roundprob(sum(length2weight(age2length(ages))*x*fecundity)))
    eggs <- matrix(eggs,ncol=1)
    return(eggs)
}
