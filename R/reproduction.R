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
#' @param breeding_near
#' @param p
#'
#' @return
#' @export
#'
#' @examples
reproduction <- function(fish,fecundity,age_mat_steepness,age_mat_sigmoid,l_to_w_int,l_to_w_power,Linf_mean,Linf_SD,k_mean,k_SD,t0,breeding_near,p){
    # generate eggs
    recruits <- fish2eggs(fish,fecundity=fecundity,age_mat_steepness=age_mat_steepness,age_mat_sigmoid=age_mat_sigmoid,l_to_w_int=l_to_w_int,l_to_w_power=l_to_w_power,Linf_mean=Linf_mean,Linf_SD=Linf_SD,k_mean=k_mean,k_SD=k_SD,t0=t0)

    # relocate eggs to nearest breeding ground
    recruits <- data.frame(r=recruits,b=breeding_near,polys=p$Breeding*breeding_near!=0) %>%
        group_by(b) %>%
        summarize(rec=sum(r),poly=sum(polys)) %>%
        mutate(recruits=rec/poly) %>%
        ungroup() %>%
        roundprob()
    recruits <- (left_join(data.frame(b=breeding_near),recruits,by="b")*p$Breeding) %>%
        dplyr::select(recruits) %>%
        unlist() %>%
        matrix()
}
