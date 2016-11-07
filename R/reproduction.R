#' Fish reproduction
#'
#' Adult fish will release eggs in the nearest breeding ground. Number of eggs produced is based on the age to length to weight relationships, the age at maturity, and fecundity levels.
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
#' @param domain
#' @param breeding_near vector of nearest Breeding
#' @inheritParams dispersal
#' @inheritParams age2length
#' @inheritParams length2weight
#' @inheritParams addscenario
#'
#' @return
#' @export
#'
#' @examples
#' # create a fish matrix
#' fish <- initpop(initial_abun=250*10^6,cells=length(BESTMPA_domain),maxage=50,rate=0.7)
#' # identify nearby breeding grounds
#' breeding_near <- apply(gDistance(spTransform(Breeding,proj),p,byid = T),1,which.min)
#' # reproduction
#' recruits <- reproduction(fish,fecundity=5e+05,age_mat_steepness=2.5,age_mat_sigmoid=4,l_to_w_int=1.1e-05,l_to_w_power=2.91,Linf_mean=112.03,Linf_SD=5.336735,k_mean=0.13,k_SD=0.01071429,t0=0.18,breeding_near=breeding_near,domain=BESTMPA_domain)
reproduction <- function(fish,fecundity,age_mat_steepness,age_mat_sigmoid,l_to_w_int,l_to_w_power,Linf_mean,Linf_SD,k_mean,k_SD,t0,breeding_near,domain){
    require(tidyverse)
    # generate eggs
    recruits <- fish2eggs(fish,fecundity=fecundity,age_mat_steepness=age_mat_steepness,age_mat_sigmoid=age_mat_sigmoid,l_to_w_int=l_to_w_int,l_to_w_power=l_to_w_power,Linf_mean=Linf_mean,Linf_SD=Linf_SD,k_mean=k_mean,k_SD=k_SD,t0=t0)

    # relocate eggs to nearest breeding ground
    recruits <- data.frame(r=recruits,b=breeding_near,polys=domain$Breeding*breeding_near!=0) %>%
        group_by(b) %>%
        summarize(rec=sum(r),poly=sum(polys)) %>%
        mutate(recruits=rec/poly) %>%
        ungroup() %>%
        roundprob()
    recruits <- (left_join(data.frame(b=breeding_near),recruits,by="b")*domain$Breeding) %>%
        dplyr::select(recruits) %>%
        unlist() %>%
        matrix()
}
